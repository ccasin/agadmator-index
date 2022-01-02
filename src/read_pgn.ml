open !Core
open Pgn
module In = In_channel

(***************************************************************************)
(**** Input management ****)

(* managing the in_channel to parse one pgn at a time is awful.  But there
   are a couple reasons to do it:
   1) The memory situation is getting dicey on my laptop, when parsing the
      whole file at once.  This allows us to incrementally process one game
      at a time or a smaller sequence of games at a time.
   2) Menhir error reporting sucks, so this will help locate parse error.

  This needs to be reworked to use big buffers, or whatever, for even slightly
  larger game databases.

 *)

(* Get a string containing just the next game in the input channel.
   - already_read is everything we've read from the input channel
     that hasn't yet been part of a returned game.
   - channel is the input channel
   - returns None if there are no games left, and the next game string
     otherwise *)
let next_game_string (already_read : string) (channel : In.t)
    : (string*string) option =
  (* Check if we've read enough already, and return the first game and the rest
     of the read input if so. *)
  let read_enough (s : string) : (string * string) option =
    let second_game : int option =
      String.substr_index s ~pattern:"\n[Event "
    in
    match second_game with
    | Some i -> Some (String.prefix s (i+1), String.drop_prefix s (i+1))
    | None ->
       if Int64.(>=) (In.pos channel) (In.length channel)
       then if String.length s > 0 then Some (s,"")
            else None
       else None
  in
  let rec get_game (already_read : string) : (string * string) option =
    match read_enough already_read with
    | Some g -> Some g
    | None ->
       if Int64.equal (In.pos channel) (In.length channel) then None
       else
         begin
           let read_len : int =
             Int64.(to_int_exn
               (min (of_int 10000) (In.length channel - In.pos channel)))
           in
           let buffer = Bytes.create read_len in
           let () =
             In.really_input_exn channel ~buf:buffer ~pos:0 ~len:read_len
           in
           get_game (already_read ^ (Bytes.to_string buffer))
         end
  in
  get_game already_read

(* (process : string -> pgn -> unit) is called for each game, with the full game
   string and the parsed pgn data *)
let parse_games
      ?(on_complete : unit -> unit = fun _ -> ())
      ?(timing_update_frequency:int = 50000)
      (filename : string) (process : string -> pgn -> unit) : unit =
  let channel : In.t = In.create filename in
  let start_time : Time.t = Time.now () in
  let rec go (already_read : string)
             (count : int) =
    match next_game_string already_read channel with
    | None -> ()
    | Some (g,already_read) ->
       begin
         let pgn =
           try Pgn_parse.only_pgn Pgn_lex.read (Lexing.from_string g) with
           | e ->
              begin
                Printf.printf "Exception: %s\n" (Exn.to_string e);
                Printf.printf "Error in game (%d):\n%s\n\nTokens:\n"
                  (String.length g) g;
                (* get_tokens g; *)
                exit 1
              end
         in
         process g pgn;
         let count = count + 1 in
         if Int.(count % timing_update_frequency = 0) then
           begin
             let time_now : Time.t = Time.now () in
             let time_since_start : Time.Span.t =
               Time.diff time_now start_time
             in
             let span_seconds : float = Time.Span.to_sec time_since_start in
             Printf.printf "Processed %d games over %f seconds (%f games/s)\n%!"
               count span_seconds Float.(of_int count / span_seconds)
           end
         else ();
         go already_read count
       end
  in
  go "" 0;
  In.close channel;
  on_complete ()
