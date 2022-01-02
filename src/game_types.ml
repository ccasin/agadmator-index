open !Core
open Bin_prot.Std

type piece =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
[@@deriving eq, sexp, compare, hash, bin_io]

let piece_to_string (p : piece) : string =
  match p with
  | Pawn -> "pawn"
  | Rook -> "rook"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Queen -> "queen"
  | King -> "king"

let piece_abbrev_move (p : piece) : string =
  match p with
  | Pawn -> ""
  | Rook -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Queen -> "Q"
  | King -> "K"

type color = White | Black
[@@deriving eq, sexp, compare, hash, bin_io]

let color_to_string (c : color) : string =
  match c with
  | White -> "white"
  | Black -> "black"

let color_swap (c : color) : color =
  match c with
  | White -> Black
  | Black -> White


type file =
  | FA
  | FB
  | FC
  | FD
  | FE
  | FF
  | FG
  | FH
[@@deriving eq, sexp, compare, hash, bin_io]

(* Why didn't I just use ints? *)
let inc_file (f : file) : file option =
  match f with
  | FA -> Some FB
  | FB -> Some FC
  | FC -> Some FD
  | FD -> Some FE
  | FE -> Some FF
  | FF -> Some FG
  | FG -> Some FH
  | FH -> None

let dec_file (f : file) : file option =
  match f with
  | FA -> None
  | FB -> Some FA
  | FC -> Some FB
  | FD -> Some FC
  | FE -> Some FD
  | FF -> Some FE
  | FG -> Some FF
  | FH -> Some FG

let file_to_string (f : file) : string =
  match f with
  | FA -> "a"
  | FB -> "b"
  | FC -> "c"
  | FD -> "d"
  | FE -> "e"
  | FF -> "f"
  | FG -> "g"
  | FH -> "h"

type rank =
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
[@@deriving eq, sexp, compare, hash, bin_io]

let inc_rank (r : rank) : rank option =
  match r with
  | R1 -> Some R2
  | R2 -> Some R3
  | R3 -> Some R4
  | R4 -> Some R5
  | R5 -> Some R6
  | R6 -> Some R7
  | R7 -> Some R8
  | R8 -> None

let dec_rank (r : rank) : rank option =
  match r with
  | R1 -> None
  | R2 -> Some R1
  | R3 -> Some R2
  | R4 -> Some R3
  | R5 -> Some R4
  | R6 -> Some R5
  | R7 -> Some R6
  | R8 -> Some R7

let rank_to_string (r : rank) : string =
  match r with
  | R1 -> "1"
  | R2 -> "2"
  | R3 -> "3"
  | R4 -> "4"
  | R5 -> "5"
  | R6 -> "6"
  | R7 -> "7"
  | R8 -> "8"

type square = file * rank
[@@deriving eq, sexp, compare, hash, bin_io]

let square_to_string (f,r : square) : string =
  file_to_string f ^ rank_to_string r


(* Position is:
     (rank - 1) * 8 + (file - 1)
  (Imagining the files are numbered a -> 1, b -> 2, ...) *)
type board = (piece * color) option Array.t
  [@@deriving sexp, compare, bin_io]

let equal_board (b1 : board) (b2 : board) : bool =
  Array.equal
    (Option.equal (fun (p1,c1) (p2,c2) -> equal_piece p1 p2
                                       && equal_color c1 c2))
    b1 b2

let piece_abbrev_board (p,c : piece * color) : string =
  let s =
    match p with
    | Pawn -> "P"
    | _ -> piece_abbrev_move p
  in
  if equal_color c Black then String.lowercase s else s

let board_to_string (b : board) : string =
  let rank_to_string (r : int) : string =
    let rec accum_rank (f : int) : string =
      if f = 8 && r > 0 then "/"
      else if f = 8 then ""
      else
        match b.(r*8 + f) with
        | None ->
           begin
             let rec num_empty_sqs (f' : int) : int =
               if f' = 8 || Option.is_some b.(r*8 + f') then
                 f' - f
               else
                 num_empty_sqs (f' + 1)
             in
             let empties : int = num_empty_sqs (f + 1) in
             Int.to_string empties ^ accum_rank (f + empties)
           end
        | Some p -> piece_abbrev_board p ^ accum_rank (f + 1)
    in accum_rank 0
  in
  let rec accum_board (r : int) : string =
      (rank_to_string r)
    ^ (if r = 0 then "" else accum_board (r-1))
  in
  accum_board 7

(* Abstract positions to array indices *)
let rank_to_idx (r : rank) : int =
  match r with
  | R1 -> 0
  | R2 -> 1
  | R3 -> 2
  | R4 -> 3
  | R5 -> 4
  | R6 -> 5
  | R7 -> 6
  | R8 -> 7

let file_to_idx (f : file) : int =
  match f with
  | FA -> 0
  | FB -> 1
  | FC -> 2
  | FD -> 3
  | FE -> 4
  | FF -> 5
  | FG -> 6
  | FH -> 7

let square_to_idx (f,r) : int = (rank_to_idx r * 8) + file_to_idx f

let idx_to_rank (i : int) : rank =
  match i with
  | 0 -> R1
  | 1 -> R2
  | 2 -> R3
  | 3 -> R4
  | 4 -> R5
  | 5 -> R6
  | 6 -> R7
  | 7 -> R8
  | _ -> failwith (Printf.sprintf "Invalid numerical rank %d" i)

let idx_to_file (i : int) : file =
  match i with
  | 0 -> FA
  | 1 -> FB
  | 2 -> FC
  | 3 -> FD
  | 4 -> FE
  | 5 -> FF
  | 6 -> FG
  | 7 -> FH
  | _ -> failwith (Printf.sprintf "Invalid numerical file %d" i)

let idx_to_square (i : int) : square =
  (idx_to_file (i % 8), idx_to_rank (i / 8))
