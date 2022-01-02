(* This module defines all the novelty stats and the logic for updating them.

   The main interface is three functions:

     init : int -> unit

     process_game : string -> pgn -> unit

     finish : unit -> unit

   You must call init before you start processing games!  Pass it some number
   that is more than the total number of games.  If the number is smaller than
   the number of games, you're gonna have a bad time.

   After processing the game, call finish to print stats.

   I don't know why I'm writing documentation for the interface that I have
   already used and no one else will look at.

   We're tracking things very effectfully.  The transposition table is naturally
   effectful.  I could eliminate some of the refs and thread some state through
   functionally, but I think for a program like this where I'm processing a
   large number of small bits of data, performance is already going to be
   massively dominated by GC time, and state passing or a state monad will just
   add to that. *)

open !Core
module H = Hashtbl
module OA = Option_array
module Out = Out_channel

open Pgn
open Game_logic
module R = Read_pgn
module P = Position

exception NotSorted of string

(******************* State ****************************)
(******************************************************)

(* This is the big transposition table.
   The keys are FEN strings (minus the last halfmove clock and fullmove number.
   The values are the unique identifier for the game.

   Using FEN strings means we aren't using zobrist hashing, which maybe is
   unfortunate for collisions?  I could try to measure that.  But we are
   storing way less data (~3x reduction in memory usage). *)
let transposition_table : (string, int) H.t =
  H.create ~size:10000000 (module String)

(* This is a map of every game we've processed so far, in order.  The game's
   index in this array serves as its unique identifier in other data structures
   (and can be determined externally from the pgn file).

   We don't actually need to save any info about the game for the novelty
   calculations, because we've presorted the list of games.  But keeping around
   some info allows us to sanity check the sort.  This could be dropped and
   would reduce memory usage *)
let game_map : game_info OA.t ref = ref OA.empty

(* The whole point of this module is to compute the novelty_map: an array
   of the novelties in each game (indexed by that game's ID, which is also
   its index in the game map *)
let novelty_map : int list A.t ref = ref (A.create ~len:0 [])

let games : int ref = ref 0
let novelties : int ref = ref 0
let ambiguities : int ref = ref 0

let init (sz : int) : unit =
  game_map := OA.create ~len:sz;
  novelty_map := A.create ~len:sz []

(******************* Logic ****************************)
(******************************************************)

let add_novelties (g_id : int) (ns : int list) =
  A.unsafe_set !novelty_map g_id ns;
  novelties := !novelties + List.length ns

exception Illegal of int list * int list

let print_update () : unit =
  let ambs = (* List.length *) !ambiguities in
  let () =
    (* Make true to print GC stats.  Expensive! *)
    if false then
      let start_time : Time.t = Time.now () in
      Gc.compact ();
      let end_time : Time.t = Time.now () in
      Printf.printf "  GC took %f seconds.\n%!"
        (Time.Span.to_sec (Time.diff end_time start_time))
    else ()
  in
  Printf.printf "  Novelties: %d\n  Positions: %d\n  Ambiguities: %d (%f)\n"
    !novelties (H.length transposition_table)
    ambs Float.(of_int ambs / of_int !novelties)


let process_game_exn (g_id : int) (p : pgn) : unit =
  let position : P.t = (P.create ()) in
  let gi = p.game_info in
  OA.unsafe_set_some !game_map g_id gi;
  let process_move
        (g, mv_num, known, w_ns, b_ns
           : P.t * int * bool * int list * int list)
        (mv : move) : (P.t * int * bool * int list * int list) =
    (* The state we're accumulating is:
       g - the position before mv
       mv_num - the half-move number of mv
       known - whether the position g was known before this game
       w_ns - the half-move numbers where white played novelties
       b_ns - the half-move numbers where white played novelties *)
    match make_move g mv with
    | MIllegal s ->
       begin
         Printf.printf "Illegal move %s in game %s (%s).\n"
           (move_to_string mv) (condensed_game_info gi)
           s;
         raise (Illegal (w_ns,b_ns))
       end
    | MMoved (g',_) ->
       let tt_key : string = P.to_string g' in
       begin
         match (H.find transposition_table tt_key,known) with
         | None, false ->
            begin
            (* This position is unknown, but the last position was also unknown,
               so it's not a novelty.  Just add it to the transposition
               table. *)
              let _ = H.add transposition_table ~key:tt_key ~data:g_id in
              (g', mv_num + 1, false, w_ns, b_ns)
            end
         | None, true ->
            begin
            (* This position is unknown, and the last position was known.
               novelty! *)
              let _ = H.add transposition_table ~key:tt_key ~data:g_id in
              let w_ns, b_ns =
                if mv_num % 2 = 1
                then (mv_num :: w_ns, b_ns)
                else (w_ns, mv_num :: b_ns)
              in
              (g', mv_num + 1, false, w_ns, b_ns)
            end
         | Some first_g_id, false ->
            begin
              (* This position was known, and the last position was unknown.
                 So, this wouldn't have been a novelty even if it were unknown.
                 We've transposed back to a known position.

                 A special case: If this position was seen before, but only in a
                 prior move of the current game, it doesn't count as a
                 previously known position.  If you're already in unknown
                 territory, you don't get credit for another novelty by
                 repeating moves and making a different choice the second time.
               *)
              (g', mv_num + 1, not (first_g_id = g_id), w_ns, b_ns)
            end
         | Some first_g_id, true ->
            begin
            (* This position was known, and so was the last one.  For the
               ambiguity stats, we check if the first occurance of the position
               is ambiguously dated wrt the current game (but was not exactly
               the current game). *)
              let first_gi : game_info =
                OA.unsafe_get_some_exn !game_map first_g_id
              in
              let order = game_novelty_ordering first_gi gi in
              let ambiguous =
                match order with
                | _ when order < 0 -> false
                | 0 when g_id = first_g_id -> false
                | 0 -> true
                | _ ->
                   let err =
                     Printf.sprintf
                       "The position after %d mvs in game:\n  %s\nOccurs in \
                        the future in game:\n  %s"
                       mv_num (condensed_game_info gi)
                       (condensed_game_info first_gi)
                   in
                   raise (NotSorted err)
              in
              (if ambiguous
               then ambiguities := !ambiguities + 1
               else ());
              (g', mv_num + 1, true, w_ns, b_ns)
            end
       end
  in
  let (_, _, _, w_ns, b_ns) =
    try
      List.fold_left p.moves ~init:(position,1,true,[],[])
        ~f:process_move
    with
      Illegal (w_ns, b_ns) -> (position, 0, true, w_ns, b_ns)
  in
  let w_ns = List.sort ~compare:Int.compare w_ns in
  let b_ns = List.sort ~compare:Int.compare b_ns in
  let ns = List.merge w_ns b_ns ~compare:Int.compare in
  games := !games + 1;
  if !games % 50000 = 0 then
    print_update ()
  else ();
  if List.length ns > 0 then
    add_novelties g_id ns
  else ()

let count : int ref = ref 0

let process_game (_ : string) (p : pgn) : unit =
  try
    (* Skip games with 0 or 1 moves.  I guess these are forfeits? *)
    if List.length p.moves > 1 then
      begin
        process_game_exn !count p;
        count := !count + 1
      end
    else
      ()
  with
  | NotSorted s ->
     begin
       Printf.printf "Exception NotSorted: %s\n" s;
       exit 1
     end
  | exn ->
     begin
       Printf.printf "Unknown exception: %s\n" (Exn.to_string exn);
       exit 1
     end

let write_novelty_map filename : unit =
  let nm_size = A.bin_size_t (List.bin_size_t Int.bin_size_t)
                  !novelty_map in
  let nm_buf = Bin_prot.Common.create_buf nm_size in
  let _ = A.bin_write_t (List.bin_write_t Int.bin_write_t)
            nm_buf ~pos:0 !novelty_map in
  let nm_bytes : Bytes.t = Bytes.create nm_size in
  Bin_prot.Common.blit_buf_bytes nm_buf nm_bytes ~len:nm_size;
  let out : Out.t = Out.create ~fail_if_exists:true filename in
  Out.output_bytes out nm_bytes;
  Out.close out;
  Printf.printf "\nWrote novelty map to %s.\n" filename

let finish (nm_filename:string) () : unit =
  write_novelty_map nm_filename


let main () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 3
  then
    begin
      Format.printf "I need exactly two arguments:\n 1) the database file \
                     name\n 2) An upper bound on the number of games\n";
      exit 1
    end
  else
    begin
      let filename = argv.(1) in
      let count_string = argv.(2) in
      let count =
        try Int.of_string count_string with
        | _ -> Printf.printf "Error second argument must be an integer.\n";
               exit 1
      in
      init count;
      let nm_filename = filename ^ ".novelties" in
      let () =
        match Sys.file_exists nm_filename with
        | `Yes | `Unknown ->
           begin
             Printf.printf "Error: %s already exists.  Terminating.\n"
               nm_filename;
             exit 1
           end
        | `No -> ()
      in
      R.parse_games ~on_complete:(finish nm_filename) filename process_game
    end

let () = main ()
