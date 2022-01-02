open !Core
open Pgn
open Game_logic
open Read_pgn
module In = In_channel

(* Some debugging functions I'm not currently using *)

let get_tokens (input : string) : unit =
  let buf = Lexing.from_string input in
  let tokens = Pgn_parse.toks Pgn_lex.read buf in
  List.iter tokens
    ~f:(fun t -> Format.printf "%s;  " (Pgn_lex.token_to_string t))


let print_tokens (filename : string) =
  let channel : In.t = In.create filename in
  let contents : string = In.input_all channel in
  let () = In.close channel in
  let length : int = String.length contents in
  Printf.printf "Big length: %d\n" length;
  get_tokens contents

(********************* Sortedness stuff ************************************)
(***************************************************************************)

(* NOTE: This doesn't really check if the list is sorted because
   game_novelty_ordering is a only partial order *)
let parse_check_sorted (filename : string) : unit =
  let parsed : game_info list ref = ref [] in
  let process (_ : string) (p : pgn) : unit =
    parsed := p.game_info :: !parsed
  in
  parse_games filename process;
  let games = !parsed in
  Printf.printf "\n\n%d games\n\n" (List.length games);
  match List.is_sorted (List.rev games) ~compare:game_novelty_ordering with
  | true -> Format.printf "Sorted!\n"
  | false -> Format.printf "Not sorted!\n"


(******************** Validation test **************************************)
(***************************************************************************)

exception IllegalMove

let validate_game (_ : string) (pgn : pgn) : unit =
  let game_state : P.t ref = ref (P.create ()) in
  try
    List.iter pgn.moves
      ~f:(fun m ->
        match make_move !game_state m with
        | MIllegal s ->
           begin
             Printf.printf "Illegal move %s (%s) in game:\n  %s\n"
               (move_to_string m) s
               (condensed_game_info pgn.game_info);
             raise IllegalMove
           end
        | MMoved (g,_) -> game_state := g)
  with
    IllegalMove -> ()

let validate_games (filename : string) : unit =
  parse_games filename validate_game


(***************************************************************************)
(***************************************************************************)


let parse_games_naive (filename : string) =
  let channel : In.t = In.create filename in
  let buf = Lexing.from_channel channel in
  let pgns = Pgn_parse.pgns Pgn_lex.read buf in
  let () = In.close channel in
  Printf.printf "Number of games: %d\n" (List.length pgns) (*;
  Printf.printf "Games: ";
  List.iter pgns ~f:(fun p -> Printf.printf "\n%s\n" (pgn_to_string p)) *)
