open !Core
open Game_types
module ZC = Zobrist_constants

(* I was originally going to keep both a board array and piece lists, but I got
   lazy *)
type t = {
    (* Game state *)
    board : board;
    turn : color;

    (* this is the square behind a pawn that was just moved two spaces *)
    ep_file : file option;

    (* Castling rights, by color and side *)
    can_castle_wk : bool;
    can_castle_wq : bool;
    can_castle_bk : bool;
    can_castle_bq : bool;

    (* piece lists *)
    (*
    pawns_w : pos list;
    rooks_w : pos list;
    knights_w : pos list;
    bishops_w : pos list;
    queen_w : pos option;
    king_w : pos;

    pawns_b : pos list;
    rooks_b : pos list;
    knights_b : pos list;
    bishops_b : pos list;
    queen_b : pos option;
    king_b : pos;
    *)
  }
[@@deriving sexp, compare, bin_io]

let equal (p1 : t) (p2 : t) =
     equal_board p1.board p2.board
  && equal_color p1.turn p2.turn
  && Option.equal equal_file p1.ep_file p2.ep_file
  && Bool.equal p1.can_castle_wk p2.can_castle_wk
  && Bool.equal p1.can_castle_wq p2.can_castle_wq
  && Bool.equal p1.can_castle_bk p2.can_castle_bk
  && Bool.equal p1.can_castle_bq p2.can_castle_bq

let create () : t =
  { board = Array.of_list
     [Some (Rook, White); Some (Knight, White); Some (Bishop, White);
      Some (Queen, White); Some (King, White); Some (Bishop, White);
      Some (Knight, White); Some (Rook, White);

      Some (Pawn, White); Some (Pawn, White); Some (Pawn, White);
      Some (Pawn, White); Some (Pawn, White); Some (Pawn, White);
      Some (Pawn, White); Some (Pawn, White);

      None; None; None; None; None; None; None; None;
      None; None; None; None; None; None; None; None;
      None; None; None; None; None; None; None; None;
      None; None; None; None; None; None; None; None;

      Some (Pawn, Black); Some (Pawn, Black); Some (Pawn, Black);
      Some (Pawn, Black); Some (Pawn, Black); Some (Pawn, Black);
      Some (Pawn, Black); Some (Pawn, Black);

      Some (Rook, Black); Some (Knight, Black); Some (Bishop, Black);
      Some (Queen, Black); Some (King, Black); Some (Bishop, Black);
      Some (Knight, Black); Some (Rook, Black)];

    turn = White;
    ep_file = None;

    can_castle_wk = true;
    can_castle_wq = true;
    can_castle_bk = true;
    can_castle_bq = true;

    (* piece lists *)
    (*
    pawns_w = [(FA,R2);(FB,R2);(FC,R2);(FD,R2);
               (FE,R2);(FF,R2);(FG,R2);(FH,R2)];
    rooks_w = [(FA,R1);(FH,R1)];
    knights_w = [(FB,R1);(FG,R1)];
    bishops_w = [(FC,R1);(FF,R1)];
    queen_w = Some (FD,R1);
    king_w = (FE,R1);b

    pawns_b = [(FA,R7);(FB,R7);(FC,R7);(FD,R7);
               (FE,R7);(FF,R7);(FG,R7);(FH,R7)];
    rooks_b = [(FA,R8);(FH,R8)];
    knights_b = [(FB,R8);(FG,R8)];
    bishops_b = [(FC,R8);(FF,R8)];
    queen_b = Some (FD,R8);
    king_b = (FE,R8)
    *)
  }

(* Everything below here is for zobrist hashing.

   This would be more efficient with move lists, and the original plan was to
   avoid hashing whole positions by just tracking updates anyway. But that's
   hard to do with the hashtbl interface, and I'm not writing my own hash
   table for this ridiculous project.
 *)


(* There are 12 piece types (the 6 pieces for each color), and 64 squares.
   This means we have 12*64 = 768 hashes.  These are in a big array, so
   we need a way to find the index of the hash from the square and the
   pieces.

   Squares are already numbered from 0 to 63.  We also number piece types, and
   then the index is:
         square * 12 + piece_type
 *)
let piece_to_int (p,c : piece * color) : int =
  let pi =
    match p with
    | Pawn -> 0
    | King -> 1
    | Queen -> 2
    | Rook -> 3
    | Bishop -> 4
    | Knight -> 5
  in
  match c with
  | White -> pi
  | Black -> pi + 6

let add_square (sq : int) (h : int) (o : (piece * color) option) : int =
  match o with
  | None -> h
  | Some p -> Int.bit_xor h (ZC.pieces.((sq * 12) + piece_to_int p))

let hash (g : t) :  int =
  let h = Array.foldi g.board ~init:0 ~f:add_square in
  let h = match g.turn with
    | White -> h
    | Black -> Int.bit_xor h ZC.black_to_move
  in
  let h = match g.ep_file with
    | None -> h
    | Some f -> Int.bit_xor h ZC.ep_file.(file_to_idx f)
  in
  let h = if g.can_castle_wk then Int.bit_xor h ZC.white_castle_ks else h in
  let h = if g.can_castle_wq then Int.bit_xor h ZC.white_castle_qs else h in
  let h = if g.can_castle_bk then Int.bit_xor h ZC.black_castle_ks else h in
  let h = if g.can_castle_bq then Int.bit_xor h ZC.black_castle_qs else h in
  h

let to_string (p : t) : string =
    (board_to_string p.board)
  ^ " "
  ^ (match p.turn with
     | White -> "w"
     | Black -> "b")
  ^ " "
  ^ (if not (   p.can_castle_wk || p.can_castle_wq
             || p.can_castle_bk || p.can_castle_bq)
     then "-"
     else (  (if p.can_castle_wk then "K" else "")
           ^ (if p.can_castle_wq then "Q" else "")
           ^ (if p.can_castle_wk then "k" else "")
           ^ (if p.can_castle_wq then "q" else "")))
  ^ " "
  ^ (match p.ep_file, p.turn with
     | None,_ -> "-"
     | Some f,White -> file_to_string f ^ "6"
     | Some f,Black -> file_to_string f ^ "3")
