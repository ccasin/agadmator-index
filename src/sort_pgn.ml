open !Core
open Pgn
open Read_pgn
module A = Array
module In = In_channel
module Out = Out_channel

exception InvalidDate of game_info

(* Games are only partially ordered.  For example, there are games with
   ambiguous dates, multiple events on the same day, and events with no round
   data.  Typical sort algorithms don't work on partial orders!  We need
   "topological sort" (which comes from directed graphs, where the edges form
   the partial order).

   Unfortunately, libraries for topological sort are built for the graph use
   case and expect to be given a graph structure with edges rather than a
   compare function.  Testing, it seems that even if we break things up into
   buckets by day and sort those buckets, constructing the graph for the larger
   buckets seems to take too long to be feasible.

   Worse: breaking things up into buckets by day or by month and sorting them
   individually is actually just straight incorrect.  The problem is the games
   with unknown days or months.  The natural thing is to put them into their own
   bucket and count this bucket as happening before or after games with more
   specific dates.  But either choice (before or after) would be wrong!  Suppose
   there is some event where we know round 2 took place on the 10th of the
   month, round 10 took place on the 20th, but we don't know the day of the 5th
   round.  We can't count it as happening before the first day of the month or
   after the last day of the month, because we know it takes plce between the
   10th and 20th.  Annoyingly, this actually happens in the data!

   So what to do?  Well, we could edit the games with unknown dates to be as
   specific as possible, based on other games in the same events, and then we'd
   be OK to count the remaining games with ambiguous as happening, say, after
   games with more specific dates.  But I don't want to edit the dates in this
   way because it might mess my my ambiguity stats a bit.

   Instead: I've implemented a kind of bucket sort with inference steps.

   1) We group each year into events, and process later events first.

   2) We bucket sort each event individually, putting each game from that event
   into its day in the year as specifically as possible.

   3) (Month inference): We look at games from this event with unknown months
   and check if there are any games in later rounds of the event that have a
   known month.  If so, we move the ambiguous game to right before the game it
   must have happened before.

   3a) Any games that are still not assigned months must have happened in a
   round after the last game with a known months, so it's fine to put them, in
   round order, at the end of the year.

   4) (Day inference - similar to month inference, but for days): We look at
   games from this event with a known month but an unknown day and check if
   there are any games in later rounds of the event that have a known day in
   this month.  If so, we move the ambiguous game to right before the game it
   must have happened before.

   4a) Any games that are still not assigned days must have happened in a round
   after the last game with a known day, so it's fine to put them, in round
   order, at the end of the month.

   5) (Event inference) We merge the buckets for this event into the other
   events we've already looked at from this year.  Sometimes the names of events
   imply things about he order of the games.  For example, I assume "London"
   happened before "London2".  So we check if there are any events in this year
   that we've already processed and have names that imply they occured later
   than this event.  Then the first game in that other event must happen after
   all games in this event.

   The event inference stuff is messy - it may be that we're guessing wrong
   about the order of events based on the names, so there is some sanity
   checking that will print warning messages if something looks off.

   It's not entirely obvious that this all comes together to result in a
   topological sort of the games.  We do additional sanity checking when
   actually computing the novelty stats.  That file will raise an exception if
   it is processing a game where a position occurs that already exists in the
   transposition table, but whose first occurance is in a "later" game according
   to `game_novelty_ordering`.  That should never occur if we've properly sorted
   according to that order.  But it took a few tries to get this "right", so the
   sanity check has been super useful.  *)


(* Aside: To easily sort based on rounds, I need a total order on rounds that is
   compatible with the partial order we user for `game_novelty_ordering`.  The
   ordering I've picked is to sort first by length the round list, then the
   usual order within the equal-length buckets.  We put games with shorter lists
   later, which has the effect of putting games with an unknown round at the
   end. *)
let compare_rounds (r1 : int list) (r2 : int list) : int =
  match Int.compare (List.length r2) (List.length r1) with
  | n when n <> 0 -> n
  | _ -> List.compare Int.compare r1 r2

let compare_game_info_rounds (gi1 : game_info) (gi2 : game_info)
    : int =
  compare_rounds gi1.gi_round gi2.gi_round

let compare_game_info_events (gi1 : game_info) (gi2 : game_info)
    : int =
  event_novelty_ordering gi1.gi_event gi2.gi_event

(* This groups the complete list of games into games for each day.  First we
   sort by year, then use List.group to break things up where the month changes.
 *)
let group_by_year (l : (game_info*string) list)
    : ((game_info*string) list list) =
  let order ({gi_date={year=y1;_};_},_) ({gi_date={year=y2;_};_},_) : int =
    Int.compare y1 y2
  in
  let l = List.stable_sort l ~compare:order in
  List.group l ~break:(fun n1 n2 -> 0 <> order n1 n2)

(* Parameters for our buckets of buckets.  A year has 12 month buckets, plus a
   list of all the games with unknown months.  A month has 32 day buckets, where
   0..30 are the real days and 31 is the "unknown" bucket.

   Additionally, during initial bucketing, we keep track of whether any games at
   all from this event have specific months or days.  If not, we can skip the
   corresponding inference steps, because they can't learn anything.  *)
let month_buckets : int = 12
let day_buckets : int = 32

let last_month : int = 11
let last_day : int = 30

let unknown_day : int = 31

(* Note: it is NOT an invariant that games in a given bucket have that specific
   corresponding date, because inference may result in games being moved up into
   buckets that are earlier than but not ordered with respect to their known
   date.  "any_known_days" and "any_known_months" mean "are their games in the
   specific day or month buckets", not are there any games whose dates actually
   give a specific day or month. *)
type month_bucket = {
    any_known_days : bool ref;
    days : (game_info * string) list A.t;
  }
type year_bucket = {
    any_known_months : bool ref;
    months : month_bucket A.t;
    unknown_month : (game_info * string) list ref;
    processed_events : (string * date) list ref
  }

let fresh_bucket () : year_bucket =
  let fresh_month (_ : int) : month_bucket =
    {any_known_days = ref false;
     days = A.create ~len:day_buckets []}
  in
  {any_known_months = ref false;
   months = A.init month_buckets ~f:fresh_month;
   unknown_month = ref [];
   processed_events = ref []}

let month_bucket_to_list ({days;_} : month_bucket) : (game_info*string) list =
  A.fold_right days ~init:[] ~f:(@)

let year_bucket_to_list ({months;unknown_month;_} : year_bucket)
    : (game_info * string) list =
  A.fold_right months ~init:!unknown_month
    ~f:(fun m acc -> month_bucket_to_list m @ acc)

let merge_games_within_event (gs : (game_info*string) list)
      (day : (game_info*string) list) : (game_info*string) list =
  List.merge gs day
    ~compare:(fun (gi1,_) (gi2,_) -> compare_game_info_rounds gi1 gi2)

let merge_game_within_event (g : game_info * string)
      (day : (game_info * string) list) : (game_info*string) list =
  merge_games_within_event [g] day

let merge_game_into_month_within_event
      (gi,s : game_info * string) (day : int)
      (month : month_bucket) : unit =
  month.days.(day) <- merge_game_within_event (gi,s) month.days.(day)

(* Here we put all the games into the most specific bucket possible based on the
   explicitly stated date - no date inference yet. *)
let initial_bucketing (gs : (game_info*string) list) : year_bucket =
  let bucket : year_bucket = fresh_bucket () in
  let bucket_game (gi,s : game_info*string) : unit =
    match gi.gi_date with
    | {month=None;_} ->
       bucket.unknown_month :=
         merge_game_within_event (gi,s) !(bucket.unknown_month)
    | {month=Some m;_} when m > last_month+1 ->
       raise (InvalidDate gi)
    | {month=Some m;day=None;_} ->
       begin
         let month : month_bucket = bucket.months.(m-1) in
         merge_game_into_month_within_event (gi,s) unknown_day month;
         bucket.any_known_months := true
       end
    | {month=Some _;day=Some d;_} when d > last_day+1 ->
       raise (InvalidDate gi)
    | {month=Some m;day=Some d;_} ->
       begin
         let month : month_bucket = bucket.months.(m-1) in
         merge_game_into_month_within_event (gi,s) (d-1) month;
         bucket.any_known_months := true;
         month.any_known_days := true
       end
  in
  List.iter gs ~f:bucket_game;
  bucket

(* Check whether a day contains a game in a later round than a the given
   game.
 *)
let day_contains_later_game (gi : game_info)
      (day : (game_info*string) list) : bool =
  List.exists day ~f:(fun (gi',_) -> compare_game_info_rounds gi gi' < 0)

(* Check whether the given month contains games in later rounds than the given
   game, and if so merges the game into the appropriate day in the month.
   Returns whether the game was merged in or not.

   allow_unknown_day will see if the game can be merged into the unknown day
   list.  We want to do this when checking if a game with an unknown month
   belongs in this month (month inference).  However, we don't want to do it if
   we're checking whether a game with an unknown day can be given a more
   specific day (day inference), because it already came from the unknown day
   list - that might result in dropping the game completely.

   Used in month and day inference. *)
let infer_game_date_in_month (gi,s : game_info * string) (month : month_bucket)
      ~(allow_unknown_day : bool) : bool =
  let rec check_days (dayi : int) : bool =
    let max_day = if allow_unknown_day then unknown_day else last_day in
    if dayi > max_day then false
    else
      if day_contains_later_game gi month.days.(dayi) then
        begin
          merge_game_into_month_within_event (gi,s) dayi month;
          true
        end
      else
        check_days (dayi + 1)
  in
  check_days 0

(* This pushes games with unknown months down into the right place,
   if there are other games in later rounds with known months. *)
let month_inference (b : year_bucket) : unit =
  let unknown_month : (game_info*string) list = !(b.unknown_month) in
  if List.length unknown_month > 0 && !(b.any_known_months) then
    (* The case where there are games with unknown months and known months, so
       it makes sense to try inference *)
    begin
      (* Returns whether we were able to merge the game into a month *)
      let merge_into_year (g : game_info * string) : bool =
        let rec check_months (monthi : int) : bool =
             monthi <= last_month
          && (   infer_game_date_in_month g b.months.(monthi)
                   ~allow_unknown_day:true
              || check_months (monthi + 1))
        in
        check_months 0
      in
      let remaining : (game_info*string) list =
        List.fold_left unknown_month ~init:[]
          ~f:(fun acc g -> if merge_into_year g then acc else (g :: acc))
      in
      b.unknown_month := List.rev remaining
    end
  else
    (* Either no game with unknown months or none with known months, so
       inference can't teach us anything - skip it *)
    ()

(* This pushes games with unknown days down into the right day,
   if there are other games in later rounds with known days. *)
let day_inference (month : month_bucket) : unit =
  let unknown_days : (game_info*string) list = month.days.(unknown_day) in
  if List.length unknown_days > 0 && !(month.any_known_days) then
    (* The case where there are games with known days and unknown days,
       so it makes sense to try inference *)
    begin
      let remaining : (game_info*string) list =
        List.fold_left unknown_days ~init:[]
          ~f:(fun acc g ->
            if infer_game_date_in_month g month ~allow_unknown_day:false
            then acc
            else (g :: acc))
      in
      month.days.(unknown_day) <- List.rev remaining
    end
  else
    (* Either no games with unknown days, or none with known days, so
       inference can't teach us anything - skip it *)
    ()

(*** Event inference ***)
(* This is where we merge an event's sorted games into the big year bucket,
   and check whether we can infer anything additional about these games dates
   based on the event name.  To support this, we keep a list of events
   we've seen before and the date of the earliest game in that event *)
let merge_month_into_month ~(acc:month_bucket) ~(to_merge:month_bucket)
    : unit =
  acc.any_known_days := !(acc.any_known_days) && !(to_merge.any_known_days);
  A.iteri to_merge.days ~f:(fun i d -> acc.days.(i) <- d @ acc.days.(i))

(* Returns None if passed an empty year (which maybe should raise an exception
   as a weird case) or, more importantly, if none of the games have a known
   month *)
let first_known_game_in_year (y : year_bucket) : date option =
  let rec find_day (mi : int) (month : month_bucket) (di : int)
          : date option =
    if di > unknown_day then None
    else
      match month.days.(di) with
      | [] -> find_day mi month (di+1)
      | (gi,_) :: _ ->
         Some {day = if di < unknown_day then Some di else None;
               month = Some mi;
               year = gi.gi_date.year}
  in
  let rec find_month (mi : int) : date option =
    if mi > last_month then None
    else
      match find_day mi y.months.(mi) 0 with
      | None -> find_month (mi+1)
      | Some d -> Some d
  in
  if !(y.any_known_months) then find_month 0 else None

let contains_unambiguously_later_game (y : year_bucket) (d : date) : bool =
  let last_game_date : date option =
    if !(y.any_known_months) then
      begin
        (* For the purposes of this function, games with known days
           are "later" than games with unknown days, because games with
           unknown days can be shifted earlier in the month. *)
        let rec last_game_in_month (m : month_bucket) (mi : int) (di : int)
                : date option =
          if di < 0 || not !(m.any_known_days) then
            if List.length m.days.(unknown_day) > 0 then
              Some {day = None; month = Some mi; year = d.year}
            else None
          else
            if List.length m.days.(di) > 0 then
              Some {day = Some di; month = Some mi; year = d.year}
            else last_game_in_month m mi (di - 1)
        in
        let rec last_game_in_year (mi : int) : date option =
          if mi < 0 then None
          else match last_game_in_month y.months.(mi) mi last_day with
               | None -> last_game_in_year (mi - 1)
               | Some d -> Some d
        in
        last_game_in_year last_month
      end
    else
      None
  in
  match last_game_date with
  | None -> false
  | Some last_game_date -> compare_date d last_game_date < 0

(* This should only be called on year_buckets that have no games that
   are unambiguously later than the date.  So we only need to look at
   games with no months, and games within the month that are later or
   have an unknown date *)
let move_games_no_later_than (y : year_bucket) (d : date option) : unit =
  match d with
  | None -> ()
  | Some {month=None;_} -> ()
  | Some {month=Some mi;day=None;_} ->
     begin
       (* All we know is the month, so we move games with an unknown month
          up to be in unknown day of that month. *)
       let games_to_move = !(y.unknown_month) in
       y.unknown_month := [];
       let new_month = y.months.(mi) in
       y.any_known_months := true;
       new_month.days.(unknown_day) <-
         merge_games_within_event games_to_move new_month.days.(unknown_day)
     end
  | Some {month=Some mi;day=Some di;_} ->
     (* We're going to move games up to a specific day.  So we get all the games
        with unknown months, and the games in later (or equal) days of that
        month *)
     begin
       y.any_known_months := true;
       let new_month : month_bucket = y.months.(mi) in
       let rec games_to_move (acc : (game_info*string) list) (di' : int) =
         if di' < di then acc else
           begin
             let moved_from_day = new_month.days.(di') in
             new_month.days.(di') <- [];
             games_to_move (merge_games_within_event moved_from_day acc)
               (di' - 1)
           end
       in
       let games_to_move = games_to_move !(y.unknown_month) unknown_day in
       y.unknown_month := [];
       new_month.days.(di) <- games_to_move;
       new_month.any_known_days :=    !(new_month.any_known_days)
                                   || (List.length games_to_move > 0);
     end

let merge_year_into_year ~(acc:year_bucket) ~(to_merge:year_bucket)
    ~(event:string) : unit =
  let earliest_later_event : (string*date) option =
    List.fold_left !(acc.processed_events) ~init:None
      (* for each previously processed event, we're checking
           1) is it a later event?
           2) is it the earliest later event we've seen so far? *)
      ~f:(fun acc (e',d') ->
            if    event_novelty_ordering event e' < 0
               && Option.value_map acc ~default:true
                    ~f:(fun (_,d) -> compare_date d' d < 0)
            then Some (e',d')
            else acc)
  in
  (* Sanity check: Are there games in the current event with KNOWN dates later
     than the supposedly later event? *)
  let games_must_be_before : date option =
    match earliest_later_event with
    | None -> None
    | Some (e,d) ->
       if contains_unambiguously_later_game to_merge d then
         begin
           if false then
             Printf.printf
               "Warning: It looks like %s should occur before %s based on the \
                event names, but it contains games after %s begins (%s).\n  \
                (Ignoring the ordering that would otherwise be implied by the \
                event names.)\n"
               event e e (date_to_string d)
           else ();
           None
         end
       else Some d
  in
  (* This moves games based on any date inferences from event names *)
  move_games_no_later_than to_merge games_must_be_before;
  (* Finally, do the merge.  We're processing events in reverse order,
     so the new games should go first in their buckets. *)
  acc.any_known_months :=
    !(acc.any_known_months) && !(to_merge.any_known_months);
  A.iteri to_merge.months
    ~f:(fun i m -> merge_month_into_month ~acc:acc.months.(i) ~to_merge:m);
  acc.unknown_month := !(to_merge.unknown_month) @ !(acc.unknown_month)

(* We attempt to process the events from "last" to "first", so that we can
   do event inference (as described in the top comment) when merging games
   into the big bucket.

   Given an event name, this checks if there are any events that appear to be
   later.  It returns a final event in this event series and a list of
   all the remaining events.

   We use both an event and a site to identify identical events, because
   the DB contains many different events with the same names but different
   sites.
 *)
let find_final_event (e,s : string * string) (idx : int)
      (l : (string * string) list) : (string*string) * (string*string) list =
  let rec later (e,s : string*string) (idx : int)
          : (string * string) * (string * string) list =
    let last_event =
      List.findi l ~f:(fun _ (e',_) -> event_novelty_ordering e' e > 0)
    in
    match last_event with
    | Some (idx',(e',s')) -> later (e',s') idx'
    | None -> ((e,s),List.take l idx @ List.drop l (idx + 1))
  in
  later (e,s) idx

let sort_year (l : (game_info*string) list) : (game_info*string) list =
  let count = List.length l in
  let y : year_bucket = fresh_bucket () in

  (* This function is called with a list of all the games, plus a previously
     constructed list of all the events that occur in the games (with duplicates
     removed).  We sort the games from the events individually, and then merge
     them *)
  let rec sort (rem_games : (game_info*string) list)
               (rem_events : (string*string) list)
    : (game_info*string) list =
    match rem_events with
    | [] -> year_bucket_to_list y
    | (e :: es) ->
       let ((final_event,final_site), rem_events) =
         find_final_event e 0 (e :: es)
       in
       let (event_games,rem_games) =
         List.partition_tf rem_games
           ~f:(fun (gi,_) ->  String.equal final_event gi.gi_event
                           && String.equal final_site gi.gi_site)
       in
       (* I think it's marginally faster to do day inference first - otherwise
          we encounter game where a month could be inferred but a day can't
          twice *)
       let event_games : year_bucket = initial_bucketing event_games in
       Array.iter event_games.months ~f:day_inference;
       month_inference event_games;
       merge_year_into_year ~acc:y ~to_merge:event_games ~event:final_event;
       (* we get the date of the earliest game in the year for event inference
          in other events this year.  It's important we do it after
          merge_year_into_year, because that does event inference on _this_
          event, and therefore might move games earlier *)
       let first_game_date : date option =
         first_known_game_in_year event_games
       in
       Option.iter first_game_date ~f:(fun d ->
           y.processed_events := (final_event,d) :: !(y.processed_events));
       sort rem_games rem_events
  in
  let date : string =
    match l with
    | [] -> "??"
    | (gi,_) :: _ ->
       match gi.gi_date with
       | {year=y;month=Some m;day=Some d} ->
          Printf.sprintf "%d-%d-%d" y m d
       | {year=y;month=Some m;_} -> Printf.sprintf "%d-%d-??" y m
       | {year=y;_} -> Printf.sprintf "%d-??-??" y
  in
  let start_time : Time.t = Time.now () in
  Printf.printf "Sorting list for %s.  Games: %d\n%!" date count;
  (* This reverses the order of the events, which is fine since we
     want to do the last event first anyway. *)
  let events : (string*string) list =
    let e_eq (e1,s1) (e2,s2) = String.equal e1 e2 && String.equal s1 s2 in
    List.fold_left l ~init:[] ~f:(fun acc ({gi_event;gi_site;_},_) ->
        if List.mem acc (gi_event,gi_site) ~equal:e_eq
        then acc
        else (gi_event,gi_site)::acc)
  in
  let sorted : (game_info*string) list = sort l events in
  let time_now : Time.t = Time.now () in
  let time_since_start : Time.Span.t =
    Time.diff time_now start_time
  in
  let span_seconds : float = Time.Span.to_sec time_since_start in
  Printf.printf "  Sorted in %f seconds (%f games/s)\n%!"
    span_seconds Float.(of_int (List.length l) / span_seconds);
  if count <> List.length sorted then
    Printf.printf "  BUG: Some games disappeared (only have %d)\n%!"
      (List.length sorted)
  else ();
  sorted

let read_sort_write (filename : string) : unit =
  let games : (game_info*string) list ref = ref [] in
  let total : int ref = ref 0 in
  let missing_month : int ref = ref 0 in
  let missing_day : int ref = ref 0 in
  parse_games filename
    (fun s pgn ->
      let gi : game_info = pgn.game_info in
      total := !total + 1;
      missing_month :=
        !missing_month + (if Option.is_some gi.gi_date.month then 0 else 1);
      missing_day :=
        !missing_day + (if Option.is_some gi.gi_date.day then 0 else 1);
      games := (gi,s) :: !games);
  Printf.printf
    "Parsed %d games.\n  Missing month: %d (%f)\n  Missing day: %d (%f)\n"
    !total !missing_month (Float.(of_int !missing_month / of_int !total))
    !missing_day (Float.(of_int !missing_day / of_int !total));
  let grouped : (game_info*string) list list = group_by_year !games in
  let sorted : (game_info*string) list =
    List.concat (List.map grouped ~f:sort_year)
  in
  Printf.printf "Sorted list contains %d games, and we started with %d\n%!"
    (List.length sorted) !total;
  Printf.printf "  %s\n%!"
    (if !total = List.length sorted then "(that's good)" else "fuck");
  let channel : Out.t = Out.create (filename ^ ".sorted") in
  List.iter sorted ~f:(fun (_,s) -> Out.output_string channel s);
  Out.close channel

let main () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 2
  then
    begin
      Format.printf "I need exactly one argument (the database file name).\n";
      exit 1
    end
  else
    begin
      let filename = argv.(1) in
      read_sort_write filename
    end

let () = main ()
