open !Core
open Game_types

type date = {
    day : int option;
    month : int option;
    year : int;
  }
[@@deriving hash, sexp, bin_io]

let compare_date d1 d2 : int =
  let years = (compare d1.year d2.year) in
  if years = 0 then
    begin
      match (d1.month, d2.month) with
      | (Some m1, Some m2) ->
         begin
           let months = compare m1 m2 in
           if months = 0 then
             begin
               match (d1.day, d2.day) with
               | (Some day1, Some day2) -> compare day1 day2
               | _ -> 0
             end
           else months
         end
      | _ -> 0
    end
  else years

let equal_date (d1 : date) (d2 : date) : bool = 0 = compare_date d1 d2

type result =
  | BlackWon
  | WhiteWon
  | Draw
  | Other

let result_to_string (r : result) : string =
  match r with
  | BlackWon -> "0-1"
  | WhiteWon -> "1-0"
  | Draw -> "1/2-1/2"
  | Other -> "*"

type side =
  | KingSide
  | QueenSide

type castle_move = {
    side : side;
    check : bool;
    mate : bool;
  }

let castle_move_to_string (cm : castle_move) =
  match cm.side with
  | KingSide -> "O-O"
  | QueenSide -> "O-O-O"

type normal_move = {
    piece : piece;
    toRank : rank;
    toFile : file;
    fromRank : rank option;
    fromFile : file option;
    check : bool;
    mate : bool;
    capture : bool;
    promote : piece option;
  }

let normal_move_to_string (nm : normal_move) : string =
    piece_abbrev_move nm.piece
  ^ (Option.value_map nm.fromFile ~default:"" ~f:file_to_string)
  ^ (Option.value_map nm.fromRank ~default:"" ~f:rank_to_string)
  ^ (if nm.capture then "x" else "")
  ^ (file_to_string nm.toFile)
  ^ (rank_to_string nm.toRank)
  ^ (Option.value_map nm.promote ~default:""
       ~f:(fun p -> "=" ^ piece_to_string p))
  ^ (if nm.check then "+" else "")
  ^ (if nm.mate then "#" else "")

type move =
  | Castle of castle_move
  | Normal of normal_move

let move_to_string (m : move) : string =
  match m with
  | Castle cm -> castle_move_to_string cm
  | Normal nm -> normal_move_to_string nm

let round_to_string (r : int list) : string =
  String.concat ~sep:"." (List.map r ~f:Int.to_string)

type game_info = {
    gi_event     : string;
    gi_site      : string;
    gi_date      : date;
    gi_round     : int list;
    gi_white     : string;
    gi_black     : string;
    gi_result    : result
  }

type pgn = {
    game_info : game_info;

    other_tags  : (string * string) list;

    moves : move list
}


(* This is for determining which of two games came first.  0 means they are the
   same game OR it's ambiguous as to which happened first.

   To determine the ordering:

   1) Look at the date.

   2) If the dates are the same, look at the event/site.  There are some events
   in the DB with names like "London" and "London2" that we can use to
   disambiguate.

   3) If the events imply an ordering, use that ordering.

   4) If the events and sites are identical, look at the round number.  We
   assume the round numbers have been processed into int lists ahead of time.
   It's important to include sites here because the DB includes many different
   events that have the same name, and are distinguished only by site. *)
let event_novelty_ordering (e1 : string) (e2 : string) : int =
  let suffix1 = String.rtake_while ~f:Char.is_digit e1 in
  let suffix2 = String.rtake_while ~f:Char.is_digit e2 in
  if String.length suffix1 = 0 && String.length suffix2 = 0 then
    0
  else
    begin
      (* OK, one of them has a suffix.  Compute the bases and see if they are
         equal.  If not, 0.  If so, compare suffixes. *)
      let e1 = String.drop_suffix e1 (String.length suffix1) in
      let e2 = String.drop_suffix e2 (String.length suffix2) in
      if String.equal e1 e2 then
        let i1 =
          if String.length suffix1 > 0 then Int.of_string suffix1 else -1
        in
        let i2 =
          if String.length suffix2 > 0 then Int.of_string suffix2 else -1
        in
        Int.compare i1 i2
      else
        0
    end

let game_novelty_ordering (gi1 : game_info) (gi2 : game_info) : int =
  match compare_date gi1.gi_date gi2.gi_date with
  | d when d <> 0 -> d
  | _ ->
     let (ev1,ev2 : string * string) = (gi1.gi_event, gi2.gi_event) in
     let (st1,st2 : string * string) = (gi1.gi_site, gi2.gi_site) in
     if    String.equal ev1 ev2
        && String.equal st1 st2 then
        let (r1,r2) = (gi1.gi_round, gi2.gi_round) in
        if Int.equal (List.length r1) (List.length r2)
        then List.compare Int.compare r1 r2
        else 0
     else if String.equal ev1 ev2 then 0 else event_novelty_ordering ev1 ev2


let date_to_string (d : date) : string =
  Printf.sprintf "%s.%s.%s"
    (Int.to_string d.year)
    (Option.value_map d.month ~default:"??" ~f:Int.to_string)
    (Option.value_map d.day   ~default:"??"  ~f:Int.to_string)


let condensed_game_info
      {gi_date; gi_event; gi_site; gi_white; gi_black; gi_round;_} : string =
      Printf.sprintf "%s v %s (%s, event: %s @ %s, round %s)"
        gi_white gi_black (date_to_string gi_date) gi_event gi_site
        (String.concat ~sep:"." (List.map gi_round ~f:Int.to_string))

let pgn_to_string (p : pgn) : string =
  let tags_to_string (tags : (string * string) list) : string =
    String.concat ~sep:"\n"
      (List.map tags
         ~f:(fun (t,v) -> Printf.sprintf "    [%s \"%s\"]" t v))
  in
  String.concat ~sep:"\n" [
      Printf.sprintf "{";
      Printf.sprintf "  event:  %s" p.game_info.gi_event;
      Printf.sprintf "  site:   %s" p.game_info.gi_site;
      Printf.sprintf "  date:   %s" (date_to_string p.game_info.gi_date);
      Printf.sprintf "  round:  %s" (round_to_string p.game_info.gi_round);
      Printf.sprintf "  white:  %s" p.game_info.gi_white;
      Printf.sprintf "  black:  %s" p.game_info.gi_black;
      Printf.sprintf "  result: %s" (result_to_string p.game_info.gi_result);
      Printf.sprintf "  other_tags: %s" (tags_to_string p.other_tags);
      Printf.sprintf "  moves:  %s" (Int.to_string (List.length p.moves));
    ]
