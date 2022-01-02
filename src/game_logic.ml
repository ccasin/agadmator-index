open !Core
open Game_types
open Pgn
module P = Position
module A = Array

exception InternalError of string

let get_piece (g : P.t) (p : file*rank) : (piece*color) option =
  g.board.(square_to_idx p)

let set_piece (g : P.t) (square : file*rank) (o : (piece*color) option) : unit =
  g.board.(square_to_idx square) <- o

(* Check that there is nothing on the provided squares *)
let squares_are_empty (g : P.t) (ps : (file * rank) list) =
  List.for_all ps ~f:(fun p -> Option.is_none (get_piece g p))


(* Normal moves may capture a piece, castling and promotion never do, and EP
   always does.  This doesn't distinguish "normal" moves from pawn promotions -
   consumers must check for pawn moves to first/last rank.

   For EP, we record the rank of the captured pawn, mainly because it's easiest
   to calculate here. *)
type move_type = MTNormal of piece option | MTCastle | MTAvoidBrick of rank

(* The *_dests functions calculate the squares that can be reached by a
   given piece on a given square.  We do not check that the piece is actually on
   that square.  And we're not considering checks - just the movement of the
   pieces.  *)
let rec left_dests (g : P.t) (f,r : file * rank) (c : color) =
  match dec_file f with
  | None -> []
  | Some f' ->
     begin
       match get_piece g (f',r) with
       | None -> (MTNormal None, f',r) :: left_dests g (f',r) c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r)]
     end

let rec right_dests (g : P.t) (f,r : file * rank) (c : color) =
  match inc_file f with
  | None -> []
  | Some f' ->
     begin
       match get_piece g (f',r) with
       | None -> (MTNormal None, f',r) :: right_dests g (f',r) c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r)]
     end

let rec down_dests (g : P.t) (f,r : file * rank) (c : color) =
  match dec_rank r with
  | None -> []
  | Some r' ->
     begin
       match get_piece g (f,r') with
       | None -> (MTNormal None, f,r') :: down_dests g (f,r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f,r')]
     end

let rec up_dests (g : P.t) (f,r : file * rank) (c : color) =
  match inc_rank r with
  | None -> []
  | Some r' ->
     begin
       match get_piece g (f,r') with
       | None -> (MTNormal None, f,r') :: up_dests g (f,r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f,r')]
     end

let rec down_left_dests (g : P.t) (f,r : file * rank) (c : color) =
  match (dec_file f, dec_rank r) with
  | (None,_) -> []
  | (_,None) -> []
  | (Some f', Some r') ->
     begin
       match get_piece g (f',r') with
       | None -> (MTNormal None,f',r') :: down_left_dests g (f',r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r')]
     end

let rec down_right_dests (g : P.t) (f,r : file * rank) (c : color) =
  match (inc_file f, dec_rank r) with
  | (None,_) -> []
  | (_,None) -> []
  | (Some f', Some r') ->
     begin
       match get_piece g (f',r') with
       | None -> (MTNormal None,f',r') :: down_right_dests g (f',r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r')]
     end

let rec up_left_dests (g : P.t) (f,r : file * rank) (c : color) =
  match (dec_file f, inc_rank r) with
  | (None,_) -> []
  | (_,None) -> []
  | (Some f', Some r') ->
     begin
       match get_piece g (f',r') with
       | None -> (MTNormal None,f',r') :: up_left_dests g (f',r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r')]
     end

let rec up_right_dests (g : P.t) (f,r : file * rank) (c : color) =
  match (inc_file f, inc_rank r) with
  | (None,_) -> []
  | (_,None) -> []
  | (Some f', Some r') ->
     begin
       match get_piece g (f',r') with
       | None -> (MTNormal None,f',r') :: up_right_dests g (f',r') c
       | Some (_,c') when equal_color c c' -> []
       | Some (p,_) -> [(MTNormal (Some p),f',r')]
     end

let orthog_dests (g : P.t) (square : file*rank) (c : color) =
  List.concat [left_dests g square c; right_dests g square c;
               up_dests g square c; down_dests g square c]

let diag_dests (g : P.t) (square : file*rank) (c : color) =
  List.concat [down_left_dests g square c; down_right_dests g square c;
               up_left_dests g square c; up_right_dests g square c]

let pawn_dests (g : P.t) (f,r : file * rank) (c : color)
    : (move_type * file * rank) list =
  let direction : rank -> rank option =
    match c with
    | White -> inc_rank
    | Black -> dec_rank
  in
  let one_forward : (move_type * file * rank) option =
    match direction r with
    | None -> raise (InternalError "Pawn on 1st or 8th rank")
    | Some r' ->
       begin
         match get_piece g (f,r') with
         | None -> Some (MTNormal None, f, r')
         | Some _ -> None
       end
  in
  let two_forward : (move_type * file * rank) option =
    if    equal_color c White
       && equal_rank r R2
       && squares_are_empty g [(f,R3);(f,R4)] then
      Some (MTNormal None, f, R4)
    else if    equal_color c Black
            && equal_rank r R7
            && squares_are_empty g [(f,R6);(f,R5)]
    then
      Some (MTNormal None, f, R5)
    else None
  in
  let capture : (move_type * file * rank) list =
    let capture_squares : (file * rank) list =
      let files = List.filter_opt [inc_file f; dec_file f] in
      match direction r with
      | None -> raise (InternalError "Pawn on first or last rank!")
      | Some r' -> List.map files ~f:(fun f -> (f,r'))
    in
    let valid_capture (f,r : file * rank) : (move_type * file * rank) option =
      match get_piece g (f,r) with
      | None ->
         begin
           match g.ep_file with
           | Some f' when equal_file f f' ->
              begin
                match r,c with
                | (R3,Black) -> Some (MTAvoidBrick R4, f, r)
                | (R6,White) -> Some (MTAvoidBrick R5, f, r)
                | _ -> None
              end
           | _ -> None
         end
      | Some (_,c') when equal_color c c' -> None
      | Some (p,_) -> Some (MTNormal (Some p), f, r)
    in
    List.filter_map capture_squares ~f:valid_capture
  in
  capture @ List.filter_opt [one_forward; two_forward]


let rook_dests (g : P.t) (square : file * rank) (c : color)
    : (move_type * file * rank) list =
  orthog_dests g square c

let knight_dests (g : P.t) (f,r : file * rank) (c : color)
    : (move_type * file * rank) list =
  let possible_dests : (file option * rank option) list =
    [(Option.bind (dec_file f) ~f:dec_file, inc_rank r);
     (Option.bind (dec_file f) ~f:dec_file, dec_rank r);
     (Option.bind (inc_file f) ~f:inc_file, inc_rank r);
     (Option.bind (inc_file f) ~f:inc_file, dec_rank r);
     (inc_file f, Option.bind (dec_rank r) ~f:dec_rank);
     (dec_file f, Option.bind (dec_rank r) ~f:dec_rank);
     (inc_file f, Option.bind (inc_rank r) ~f:inc_rank);
     (dec_file f, Option.bind (inc_rank r) ~f:inc_rank)]
  in
  let validate_dest (fo, ro : file option * rank option)
      : (move_type * file * rank) option =
    match fo,ro with
    | (Some f, Some r) ->
       begin
         match get_piece g (f,r) with
         | None -> Some (MTNormal None, f, r)
         | Some (p,c') when not (equal_color c c') ->
            Some (MTNormal (Some p), f, r)
         | _ -> None
       end
    | _ -> None
  in
  List.filter_map possible_dests ~f:validate_dest

let bishop_dests (g : P.t) (square : file * rank) (c : color)
    : (move_type * file * rank) list =
  diag_dests g square c

let queen_dests (g : P.t) (square : file * rank) (c : color)
    : (move_type * file * rank) list =
  orthog_dests g square c @ diag_dests g square c

let king_dests (g : P.t) (f,r : file * rank) (c : color)
    : (move_type * file * rank) list =
  let possible_dests : (file option * rank option) list =
    [(dec_file f, Some r);
     (dec_file f, inc_rank r);
     (Some f,     inc_rank r);
     (inc_file f, inc_rank r);
     (inc_file f, Some r);
     (inc_file f, dec_rank r);
     (Some f,     dec_rank r);
     (dec_file f, dec_rank r)]
  in
  let validate_dest (fo, ro : file option * rank option)
      : (move_type * file * rank) option =
    match fo,ro with
    | (Some f, Some r) ->
       begin
         match get_piece g (f,r) with
         | None -> Some (MTNormal None, f, r)
         | Some (p,c') when not (equal_color c c') ->
            Some (MTNormal (Some p), f, r)
         | _ -> None
       end
    | _ -> None
  in
  List.filter_map possible_dests ~f:validate_dest

let dests (g : P.t) (p : piece) (square : file * rank) (c : color)
    : (move_type * file * rank) list =
  match p with
  | Pawn   -> pawn_dests g square c
  | Rook   -> rook_dests g square c
  | Knight -> knight_dests g square c
  | Bishop -> bishop_dests g square c
  | Queen  -> queen_dests g square c
  | King   -> king_dests g square c


(* Adding and removing pieces from the board.  Add checks nothing was there, and
   remove checks something was there.  Both return an optional error - array
   updates are effectful and this doesn't touch any other fields in the record,
   so no need to return an new game. *)
let add_pieces (g : P.t) (ps : (color * piece * file * rank) list)
    : string option =
  let add_piece (err : string option) (c,p,f,r : color * piece * file * rank)
      : string option =
    match err with
    | Some s -> Some s
    | None ->
       begin
         match get_piece g (f,r) with
         | Some (p',c') ->
            Some
              (Printf.sprintf "Square %s already contains a %s %s"
                 (square_to_string (f,r)) (color_to_string c')
                 (piece_to_string p'))
         | None -> (set_piece g (f,r) (Some (p,c)); None)
       end
  in
  List.fold_left ps ~init:None ~f:add_piece

let remove_pieces (g : P.t) (ps : (color * piece * file * rank) list)
    : string option =
  let rem_piece (err : string option) (c,p,f,r : color * piece * file * rank)
      : string option =
    match err with
    | None ->
       begin
         match get_piece g (f,r) with
         | None ->
            Some (Printf.sprintf "Attempt to remove %s from empty square %s"
                    (piece_to_string p) (square_to_string (f,r)))
         | Some (p',c') ->
            if equal_color c c' && equal_piece p p' then
              (set_piece g (f,r) None; None)
            else
              Some
                (Printf.sprintf
                   "Attempt to remove a %s %s from %s, which contains a %s %s"
                   (color_to_string c) (piece_to_string p)
                   (square_to_string (f,r))
                   (color_to_string c') (piece_to_string p'))
       end
    | Some s -> Some s
  in
  List.fold_left ps ~init:None ~f:rem_piece

(* True if the current player is in check *)
let in_check (g : P.t) : bool =
  (* We iterate over the pieces of the opposite color and check if any of them
     can attack the king.  This is horribly inefficient. *)
  let attacks_king (i : int) (o : (piece * color) option) : bool =
    match o with
    | None -> false
    | Some (_,c) when equal_color c g.turn -> false
    | Some (p,c) ->
       begin
         let attacks : (move_type * file * rank) list =
           dests g p (idx_to_square i) c
         in
         List.exists attacks
           ~f:(fun (mt,_,_) -> match mt with
                               | MTNormal (Some King) -> true
                               | _ -> false)
       end
  in
  Array.existsi g.board ~f:attacks_king


(* If the move is successful, we return not just the new game, but also a list
   of all "added" and "removed" pieces for the purpose efficient zobrist hashing

   This list should have an entry for the start and end position of every moving
   piece and include any captured piece - because we xor "out" the old positions
   and "in" the new positions for the zobrist hash.

   E.g., for 1. e4 we'd have [(White,Pawn,FE,R2),(White,Pawn,FE R4)].

   If we have white playing gxh8=Q where the captured piece is a rook, we'd
   have: [(White,Pawn,FG,R7),(White,Queen,FH,R8),(Black,Rook,FH,R8)] *)
type move_result =
  | MIllegal of string
  | MMoved of P.t * (color * piece * file * rank) list

let is_mmoved (mr : move_result) : bool =
  match mr with
  | MMoved _ -> true
  | _ -> false

let castle_move_updates (g : P.t) (cm : castle_move) :
      (  (  (color * piece * file * rank) list
          * (color * piece * file * rank) list
          * P.t ) list,
         string) Either.t =
  let castle_err = "Castling blocked or pieces already moved" in
  match (g.turn, cm.side) with
  | (White,KingSide) ->
     let not_blocked : bool =
       squares_are_empty g [(FF,R1);(FG,R1)]
     in
     if g.can_castle_wk && not_blocked then
       Either.First
         [([(White,Rook,FF,R1);(White,King,FG,R1)],
           [(White,Rook,FH,R1);(White,King,FE,R1)],
           { g with
             can_castle_wk = false;
             can_castle_wq = false;
             ep_file = None})]
     else Either.Second castle_err
  | (White,QueenSide) ->
     let not_blocked : bool =
       squares_are_empty g [(FB,R1);(FC,R1);(FD,R1)]
     in
     if g.can_castle_wq && not_blocked then
       Either.First
         [([(White,Rook,FD,R1);(White,King,FC,R1)],
           [(White,Rook,FA,R1);(White,King,FE,R1)],
           { g with
             can_castle_wk = false;
             can_castle_wq = false;
             ep_file = None})]
     else Either.Second castle_err
  | (Black,KingSide) ->
     let not_blocked : bool =
       squares_are_empty g [(FF,R8);(FG,R8)]
     in
     if g.can_castle_bk && not_blocked then
       Either.First
         [([(Black,Rook,FF,R8);(Black,King,FG,R8)],
           [(Black,Rook,FH,R8);(Black,King,FE,R8)],
           { g with
             can_castle_bk = false;
             can_castle_bq = false;
             ep_file = None})]
     else Either.Second castle_err
  | (Black,QueenSide) ->
     let not_blocked : bool =
       squares_are_empty g [(FB,R8);(FC,R8);(FD,R8)]
     in
     if g.can_castle_bq && not_blocked then
       Either.First
         [([(Black,Rook,FD,R8);(Black,King,FC,R8)],
           [(Black,Rook,FA,R8);(Black,King,FE,R8)],
           { g with
             can_castle_bk = false;
             can_castle_bq = false;
             ep_file = None})]
     else Either.Second castle_err


let normal_move_updates (g : P.t) (nm : normal_move)
    : (  (  (color * piece * file * rank) list
          * (color * piece * file * rank) list
          * P.t ) list ,
         string) Either.t =
  (* A naive approach: we find the pieces of the right color, calculate their
     valid moves, and check if one matches the move we're given.  This is
     inefficient, but avoids duplicating a bunch of logic from the move 'dests'
     functions above and I'm lazy. *)
  let potential_pieces : square list =
    let rec find_pieces (acc : square list) (idx : int) : square list =
      if idx > 63 then acc else
        match g.board.(idx) with
        | Some (p,c) when equal_piece p nm.piece && equal_color c g.turn ->
           find_pieces (idx_to_square idx :: acc) (idx + 1)
        | _ -> find_pieces acc (idx+1)
    in
    find_pieces [] 0
  in

  (* The square is the source square.  We already have the destination from
            the normal_move *)
  let moves : (file*rank*move_type) list =
    List.concat_map potential_pieces
      ~f:(fun (f,r) ->
        (* Check piece source info in move *)
        match (nm.fromRank,nm.fromFile) with
        | (Some r',_) when not (equal_rank r r') -> []
        | (_, Some f') when not (equal_file f f') -> []
        | _ ->
           (* filter possible moves for ones that end at the right
                     destination *)
           List.filter_map (dests g nm.piece (f,r) g.turn)
             ~f:(fun (mt,f',r') ->
               if equal_file f' nm.toFile && equal_rank r' nm.toRank then
                 Some (f,r,mt)
               else None))
  in
  let mtnormal_updates (f_src,r_src : file*rank) (cap : piece option)
      :   (color * piece * file * rank) list
        * (color * piece * file * rank) list
        * P.t =
    let adds =
      match nm.promote with
      | Some promotion_piece ->
         [(g.turn, promotion_piece, nm.toFile, nm.toRank)]
      | None ->
         [(g.turn, nm.piece, nm.toFile, nm.toRank)]
    in
    let captures =
      match cap with
      | None -> []
      | Some p -> [(color_swap g.turn, p, nm.toFile, nm.toRank)]
    in
    let removes =
      captures @ [(g.turn, nm.piece, f_src, r_src)]
    in
    let ep_file =
      match (nm.piece,r_src,nm.toRank) with
      | (Pawn,R2,R4) -> Some (nm.toFile)
      | (Pawn,R7,R5) -> Some (nm.toFile)
      | _ -> None
    in
    let (can_castle_wk, can_castle_wq) =
      match (nm.piece, f_src, r_src) with
      | (King, FE, R1) -> (false, false)
      | (Rook, FA, R1) -> (g.can_castle_wk, false)
      | (Rook, FH, R1) -> (false, g.can_castle_wq)
      | _ -> (g.can_castle_wk,g.can_castle_wq)
    in
    let (can_castle_bk, can_castle_bq) =
      match (nm.piece,f_src, r_src) with
      | (King, FE, R8) -> (false, false)
      | (Rook, FA, R8) -> (g.can_castle_bk, false)
      | (Rook, FH, R8) -> (false, g.can_castle_bq)
      | _ -> (g.can_castle_bk,g.can_castle_bq)
    in
    (adds, removes,
     {g with
       ep_file;
       can_castle_wk; can_castle_wq;
       can_castle_bk; can_castle_bq})
  in
  let mtavoidbrick_updates (f_src,r_src : file*rank) (r_ep : rank)
      :   (color * piece * file * rank) list
          * (color * piece * file * rank) list
          * P.t
    = ([(g.turn           , Pawn, nm.toFile, nm.toRank)],
       [(g.turn           , Pawn, f_src    , r_src);
        (color_swap g.turn, Pawn, nm.toFile, r_ep)],
       {g with ep_file = None})
  in
  let mt_updates :
        (  (color * piece * file * rank) list
           * (color * piece * file * rank) list
           * P.t) list option =
    List.fold_left moves ~init:(Some [])
      ~f:(fun acc mv ->
        match (acc,mv) with
        | (None, _) -> None
        | (Some _, (_,_,MTCastle)) -> None
        | (Some acc, (f_src, r_src, MTAvoidBrick r_ep)) ->
           Some (mtavoidbrick_updates (f_src,r_src) r_ep :: acc)
        | (Some acc, (f_src, r_src, MTNormal c)) ->
           Some (mtnormal_updates (f_src,r_src) c :: acc))
  in
  match mt_updates with
  | Some us -> Either.First us
  | None -> Either.Second "Castle where unexpected or other weird move error"

let make_move (g : P.t) (m : move) : move_result =
  (* We calculate the piece additions, removals, and update other pieces of game
     state (e.g., castling rights).  We return a list and delay performing the
     actual piece movements until afterwards, because we may need to
     disambiguate based on check. . *)
  let piece_updates :
        (  (  (color * piece * file * rank) list
            * (color * piece * file * rank) list
            * P.t ) list,
         string) Either.t =
    match m with
    | Castle cm -> castle_move_updates g cm
    | Normal nm -> normal_move_updates g nm
  in
  let perform_updates (adds,removes,g) : move_result =
    match (remove_pieces g removes, add_pieces g adds) with
    | (Some s,_) -> MIllegal s
    | (None,Some s) -> MIllegal s
    | (None,None) ->
       if in_check g then
         MIllegal "King ends in check"
       else
         MMoved ({g with turn = color_swap g.turn},
                 removes @ adds)
  in
  (* XXX this match below needs to be updated for the fact that piece updates is a list now *)
  match piece_updates with
  | Either.Second s -> MIllegal s
  | Either.First [] -> MIllegal "Move is illegal"
  | Either.First [m] -> perform_updates m
  | Either.First mvs ->
     begin
     (* Here we have multiple candidate moves that look legal.  This can arise
        if the reason there are two pieces that could move to the target square
        but one would leave the king in check.  We just peform both moves and
        look at whether we're in check after each.  Could be done faster.

        We must duplicate the board in each of these games before performing the
        move - because aliasing. *)
       let performed : move_result list =
         List.map mvs ~f:(fun (adds,rems,g) ->
             perform_updates (adds,rems, {g with board = Array.copy g.board}))
       in
       match List.filter performed ~f:is_mmoved with
       | [] -> MIllegal "Move is illegal"
       | [m] -> m
       | _ -> MIllegal "Move is ambiguous"
     end
