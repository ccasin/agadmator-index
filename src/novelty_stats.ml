(* This module does the work of computing the interesting stats from the
   pre-computed map of which games have novelties. *)

open !Core
open Pgn
module P = Position
module PG = Player_groups
module H = Hashtbl
module A = Array
module In = In_channel


(* This is info we want in multiple places, so it made sense to give it nice
   names and compute it just once per game *)
type game_move_info = {
    gmi_moves : int;
    gmi_white_moves : int;
    gmi_black_moves : int;
    gmi_novelties : int;
    gmi_novelty_list : int list;
    gmi_white_novelties : int;
    gmi_black_novelties : int
}


(************************ State **************************************)
(*********************************************************************)

type player_stats = {
    total_games : int;
    total_novelties : int;
    total_moves : int;
    games_with_novelties : int;
  }

type year_stats = {
    total_games : int;
    total_novelties : int;
    total_moves : int;
    games_with_novelties : int;
}

type global_stats = {
    total_games : int ref;
    total_novelties : int ref;
    total_moves : int ref;
    games_with_novelties : int ref;
    white_novelties : int ref;
    black_novelties : int ref;
    games_with_white_novelties : int ref;
    games_with_black_novelties : int ref;
    white_moves : int ref;
    black_moves : int ref;
    top_10_games : (pgn * game_move_info) list ref
  }

let players : (string, player_stats) H.t =
  H.create ~size:500000 (module String)

let years : (int, year_stats) H.t =
  H.create ~size:500 (module Int)


let global_stats : global_stats = {
    total_games = ref 0;
    total_novelties = ref 0;
    total_moves = ref 0;
    games_with_novelties = ref 0;
    white_novelties = ref 0;
    black_novelties = ref 0;
    games_with_white_novelties = ref 0;
    games_with_black_novelties = ref 0;
    white_moves = ref 0;
    black_moves = ref 0;
    top_10_games = ref []
  }

(* This is the map of games to novelties that we computed in find_novelties *)
let novelty_map : int list A.t ref = ref (A.create ~len:0 [])

let init_player_stats : player_stats =
  { total_games = 0;
    total_novelties = 0;
    total_moves = 0;
    games_with_novelties = 0; }

let init_year_stats : year_stats =
  { total_games = 0;
    total_novelties = 0;
    total_moves = 0;
    games_with_novelties = 0;
  }

(************************ Stat computation****************************)
(*********************************************************************)


let update_year_stats (p : pgn) (gmi : game_move_info) : unit =
  let update (old_stats : year_stats option) : year_stats =
    let {total_games; total_novelties;
         total_moves; games_with_novelties} : year_stats =
      match old_stats with
      | Some ys -> ys
      | None -> init_year_stats
    in
    { total_games = total_games + 1;
      total_novelties = total_novelties + gmi.gmi_novelties;
      total_moves = total_moves + gmi.gmi_moves;
      games_with_novelties =
        games_with_novelties + (if gmi.gmi_novelties > 0 then 1 else 0) }
  in
  H.update years p.game_info.gi_date.year ~f:update

let update_player_stats (p : pgn) (gmi : game_move_info) : unit =
  let update_one_player (moves : int) (novelties : int)
        (old_stats : player_stats option) : player_stats =
    let {total_games; total_novelties; total_moves; games_with_novelties}
        : player_stats =
      match old_stats with
      | Some ps -> ps
      | None -> init_player_stats
    in
    { total_games = total_games + 1;
      total_novelties = total_novelties + novelties;
      total_moves = total_moves + moves;
      games_with_novelties =
        games_with_novelties + (if novelties > 0 then 1 else 0) }
  in
  let gi : game_info = p.game_info in
  H.update players gi.gi_white
    ~f:(update_one_player gmi.gmi_white_moves gmi.gmi_white_novelties);
  H.update players gi.gi_black
    ~f:(update_one_player gmi.gmi_black_moves gmi.gmi_black_novelties)

let update_global_stats (p : pgn) (gmi : game_move_info) : unit =
  let {total_games; total_novelties; total_moves;  games_with_novelties;
       white_novelties; black_novelties;
       games_with_white_novelties; games_with_black_novelties;
       white_moves; black_moves; top_10_games} = global_stats in

  total_games := 1 + !total_games;
  total_novelties := gmi.gmi_novelties + !total_novelties;
  total_moves := gmi.gmi_moves + !total_moves;
  games_with_novelties :=
      (if gmi.gmi_novelties > 0 then 0 else 1) + !games_with_novelties;

  white_novelties := gmi.gmi_white_novelties + !white_novelties;
  black_novelties := gmi.gmi_black_novelties + !black_novelties;
  games_with_white_novelties :=
      (if gmi.gmi_white_novelties > 0 then 1 else 0)
    + !games_with_white_novelties;
  games_with_black_novelties :=
      (if gmi.gmi_black_novelties > 0 then 1 else 0)
    + !games_with_black_novelties;

  white_moves := gmi.gmi_white_moves + !white_moves;
  black_moves := gmi.gmi_black_moves + !black_moves;

  (* Update the list of 10 games with most novelties *)
  let rec insert_top_10 (idx : int) (inserted : bool)
            (top_10 : (pgn * game_move_info) list)
          : (pgn * game_move_info) list =
    if idx > 9 then []
    else match top_10 with
         | [] -> [(p,gmi)]
         | p :: ps when inserted -> p :: insert_top_10 (idx+1) true ps
         | (p',gmi') :: ps when gmi.gmi_novelties > gmi'.gmi_novelties ->
            (p,gmi) :: insert_top_10 (idx+1) true ((p',gmi') :: ps)
         | p :: ps -> p :: insert_top_10 (idx+1) false ps
  in
  top_10_games := insert_top_10 0 false !top_10_games



let make_gmi (p : pgn) (novelties : int list) : game_move_info =
  let white_moves = (1 + List.length p.moves) / 2 in
  let black_moves = List.length p.moves - white_moves in
  let white_novelties : int =
    List.count novelties ~f:(fun n -> n%2 = 1)
  in
  let black_novelties : int =
    List.length novelties - white_novelties
  in
  { gmi_moves = List.length p.moves;
    gmi_white_moves = white_moves;
    gmi_black_moves = black_moves;
    gmi_novelties = List.length novelties;
    gmi_novelty_list = novelties;
    gmi_white_novelties = white_novelties;
    gmi_black_novelties = black_novelties }


(************************ Stat display *******************************)
(*********************************************************************)

let output_global_stats () : unit =
  let {total_games; total_novelties; total_moves;  games_with_novelties;
       white_novelties; black_novelties;
       games_with_white_novelties; games_with_black_novelties;
       white_moves;  black_moves; top_10_games} = global_stats in

  Printf.printf "\n---------- Overall Stats ----------\n";
  Printf.printf   "-----------------------------------\n\n";
  Printf.printf "%d games with novelties of %d total games (%.2f%%).\n"
    !games_with_novelties !total_games
    Float.(100. * (of_int !games_with_novelties) / (of_int !total_games));
  Printf.printf "%d novelties of %d total moves (%.1f%%).\n"
    !total_novelties
    !total_moves
    Float.(100. * (of_int !total_novelties) / (of_int !total_moves));
  Printf.printf "%f average novelties per game. \n\n"
    Float.((of_int !total_novelties) / (of_int !total_games));

  Printf.printf "For White:\n";
  Printf.printf "  %d games with novelties of %d total games (%.1f%%).\n"
    !games_with_white_novelties !total_games
    Float.(100. * (of_int !games_with_white_novelties)
           / (of_int !total_games));
  Printf.printf "  %d novelties of %d total moves (%.1f%%).\n"
    !white_novelties
    !white_moves
    Float.(100. * (of_int !white_novelties)
           / (of_int !white_moves));
  Printf.printf "  %f average novelties per game. \n\n"
    Float.(of_int !white_novelties / of_int !total_games);

  Printf.printf "For Black:\n";
  Printf.printf "  %d games with novelties of %d total games (%.1f%%).\n"
    !games_with_black_novelties !total_games
    Float.(100. * (of_int !games_with_black_novelties)
           / (of_int !total_games));
  Printf.printf "  %d novelties of %d total moves (%.1f%%).\n"
    !black_novelties
    !black_moves
    Float.(100. * (of_int !black_novelties)
           / (of_int !black_moves));
  Printf.printf "  %f average novelties per game. \n\n"
    Float.(of_int !black_novelties / of_int !total_games);

  Printf.printf "Top 10 Games: Most Novelties";
  let show_game_novelties (p,gmi : pgn * game_move_info) : unit =
    Printf.printf "\n  %d novelties: %s\n"
      gmi.gmi_novelties (condensed_game_info p.game_info);
    Printf.printf "    [%s]\n"
      (String.concat (List.map gmi.gmi_novelty_list
                        ~f:(fun n -> Int.to_string n ^ "; ")))
  in
  List.iter !top_10_games ~f:show_game_novelties


let output_player_stats () : unit =
  let player_count : int = H.length players in
  let player_list : (string * player_stats) list =
    H.to_alist players
  in
  let sizes : int list = [5;10;25;50;100;250;500;1000] in
  let players_of_size (size : int) : unit =
    let count = H.count players ~f:(fun ps -> ps.total_games >= size) in
    Printf.printf "  %4d games: %#d (%.03f%%)\n"
      size count Float.(100. * of_int count / of_int player_count)
  in
  let print_top_n (n : int) (min_games : int)
        ~(stat : player_stats -> float)
        ~(compare : float -> float -> int)
      : unit =
    let nth (n : int) (plr,stat,gms : string*float*int) : unit =
      Printf.printf "    %d. %s  (%.03f) (%d games)\n" (n+1) plr stat gms
    in
    let data : (string * float * int) list =
      List.filter_map player_list
        ~f:(fun (plr,ps) -> if ps.total_games < min_games then None
                            else Some (plr,stat ps,ps.total_games))
    in
    let data : (string * float * int) list =
      List.take
        (List.sort data ~compare:(fun (_,f1,_) (_,f2,_) -> compare f1 f2))
        n
    in
    Printf.printf "  At least %d games:\n" min_games;
    List.iteri data ~f:nth
  in
  Printf.printf "\n----------- Player Stats ----------\n";
  Printf.printf   "-----------------------------------\n\n";
  Printf.printf "Unique players: %#d\n" player_count;

  Printf.printf "Players with at least:\n";
  List.iter sizes ~f:players_of_size;

  Printf.printf "\nMost novelties per game, among players with:\n";
  List.iter sizes ~f:(fun min_games ->
      print_top_n 10 min_games
        ~stat:(fun ps ->
          Float.(of_int ps.total_novelties / of_int ps.total_games))
        ~compare:(fun f1 f2 -> Int.neg (Float.compare f1 f2)));

  Printf.printf "\nLeast novelties per game, among players with:\n";
  List.iter sizes ~f:(fun min_games ->
      print_top_n 10 min_games
        ~stat:(fun ps ->
          Float.(of_int ps.total_novelties / of_int ps.total_games))
        ~compare:Float.compare);

  Printf.printf "\nMost novelties per 100 moves, among players with:\n";
  List.iter sizes ~f:(fun min_games ->
      print_top_n 10 min_games
        ~stat:(fun ps ->
          Float.(100. * of_int ps.total_novelties / of_int ps.total_moves))
        ~compare:(fun f1 f2 -> Int.neg (Float.compare f1 f2)));

  Printf.printf "\nLeast novelties per 100 moves, among players with:\n";
  List.iter sizes ~f:(fun min_games ->
      print_top_n 10 min_games
        ~stat:(fun ps ->
          Float.(100. * of_int ps.total_novelties / of_int ps.total_moves))
        ~compare:Float.compare)


let output_year_stats () : unit =
  let decade_stats (d : int) : year_stats =
    let add_year (y : int) (acc : year_stats) : year_stats =
      match H.find years y with
      | None -> acc
      | Some ys -> {
          total_games = acc.total_games + ys.total_games;
          total_novelties = acc.total_novelties + ys.total_novelties;
          total_moves = acc.total_moves + ys.total_moves;
          games_with_novelties =
            acc.games_with_novelties + ys.games_with_novelties}
    in
    let rec go (acc : year_stats) (y : int) : year_stats =
      if y = 10 then acc else
        go (add_year (d + y) acc) (y + 1)
    in
    go init_year_stats 0
  in
  let decades : int list =
    [1800; 1810; 1820; 1830; 1840; 1850; 1860; 1870; 1880; 1890;
     1900; 1910; 1920; 1930; 1940; 1950; 1960; 1970; 1980; 1990;
     2000; 2010; 2020]
  in
  let output_decade (d : int) =
    let {total_games; total_novelties;
         total_moves; games_with_novelties} : year_stats =
      decade_stats d
    in
    Printf.printf "  %ds:  %.3f        %.2f         %7d         %.1f%%\n"
      d
      Float.(of_int total_novelties / of_int total_games)
      Float.(100. * of_int total_novelties / of_int total_moves)
      total_games
      Float.(100. * of_int games_with_novelties / of_int total_games);
  in
  Printf.printf "\n----------- Decade Stats ----------\n";
  Printf.printf   "-----------------------------------\n\n";
  Printf.printf "Stats per decade :\n";
  Printf.printf "         (n / gm)  (n / 100mvs)  (# games)  (%% of gms w/ novelties)\n";
  List.iter decades ~f:output_decade

let output_player_group_stats () :  unit =
  (* Returns the list sorted by novelties / game *)
  let gather_group_stats (group : string list) : (string * player_stats) list =
    let unsorted : (string * player_stats) list =
      List.map group ~f:(fun s -> (s,Option.value ~default:init_player_stats
                                       (H.find players s)))
    in
    List.rev (List.sort unsorted
      ~compare:(fun (_,ps1) (_,ps2) ->
        Float.(compare (of_int ps1.total_novelties / of_int ps1.total_games)
                       (of_int ps2.total_novelties / of_int ps2.total_games))))
  in
  let print_group (gs : (string * player_stats) list) : unit =
    let print_player (s, ps : string * player_stats) : unit =
      Printf.printf "%21s:   %.3f      %.2f        %4d          %.1f%%\n"
        s
        Float.(of_int ps.total_novelties / of_int ps.total_games)
        Float.(100. * of_int ps.total_novelties / of_int ps.total_moves)
        ps.total_games
        Float.(100. * of_int ps.games_with_novelties / of_int ps.total_games)
    in
    Printf.printf "                     (n / gm)  (n / 100mvs)  (# games) (%% of gms w/ novelties)\n";
    List.iter gs ~f:print_player
  in
  let champ_stats = gather_group_stats PG.champions in
  let face_stats = gather_group_stats PG.personalities in
  Printf.printf "\n----------- World Champs ----------\n";
  Printf.printf   "-----------------------------------\n\n";
  print_group champ_stats;
  Printf.printf "\n-------- Chess Personalities ---------\n";
  Printf.printf   "-----------------------------------\n\n";
  print_group face_stats


let output_stats () : unit =
  output_global_stats ();
  output_player_stats ();
  output_year_stats ();
  output_player_group_stats ()

(************************ Main ***************************************)
(*********************************************************************)

(* This reads in the pre-computed novelty data from disk *)
let load_novelties (filename : string) : unit =
  let channel : In.t = In.create filename in
  let buf_size_64 : int64 = In.length channel in
  let buf_size : int =
    match Int64.to_int buf_size_64 with
    | None ->
      Printf.printf "Error: novelty map too large (%s bytes).\n"
        (Int64.to_string buf_size_64);
      exit 1
    | Some i -> i
  in
  let buf : Bin_prot.Common.buf = Bin_prot.Common.create_buf buf_size in
  let nm_string : string = In.input_all channel in
  In.close channel;
  Bin_prot.Common.blit_string_buf nm_string buf ~len:buf_size;
  let pos_ref : int ref = ref 0 in
  let nm : int list A.t =
    A.bin_read_t ~pos_ref (List.bin_read_t Int.bin_read_t) buf
  in
  novelty_map := nm;
  Printf.printf "Read novelty map.\n"

let process_game (_ : string) (p : pgn) : unit =
  (* To match find_novelties, we skip games with 0 or 1 moves *)
  if List.length p.moves > 1 then
    begin
      let g_id : int = !(global_stats.total_games) in
      let novelties : int list = (!novelty_map).(g_id) in
      let gmi : game_move_info = make_gmi p novelties in
      update_year_stats p gmi;
      update_player_stats p gmi;
      update_global_stats p gmi
    end
  else ()

let main () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 3
  then
    begin
      Format.printf "I need exactly two arguments:\n 1) the sorted database \
                     file name\n 2) The precomputed novelty map file name.\n";
      exit 1
    end
  else
    begin
      let db_filename = argv.(1) in
      let nm_filename = argv.(2) in
      load_novelties nm_filename;
      Read_pgn.parse_games db_filename process_game ~on_complete:output_stats
    end

let () = main ()
