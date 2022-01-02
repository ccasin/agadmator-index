%{
open Core
open Game_types
open Pgn

(* All this validation code is gross, but menhir is very limited.  Should
   have gone with parser combinators, or just parsed everything as strings
   and sorted it outside the parser (ick). *)
exception MissingTag of string
exception InvalidMove of string

let get_tag (tag : string) (pairs : (string * string) list)
    : string * (string * string) list =
  let rec get_tag_helper acc pairs =
    match pairs with
    | ((t,v)::pairs) when String.equal t tag -> (v, (List.rev acc) @ pairs)
    | (p::pairs) -> get_tag_helper (p::acc) pairs
    | [] -> raise (MissingTag tag)
  in
  get_tag_helper [] pairs

(* Assuming the format is exactly YYYY.MM.DD.  ?? allowed for MM and DD *)
let parse_date (d : string) : date =
  let year = int_of_string (String.sub d ~pos:0 ~len:4) in
  let month =
    let month_substring = String.sub d ~pos:5 ~len:2 in
    if String.equal month_substring "??" then None
    else Some (int_of_string month_substring)
  in
  let day =
    let day_substring = String.sub d ~pos:8 ~len:2 in
    if String.equal day_substring "??" then None
    else Some (int_of_string day_substring)
  in
  {day;month;year}

let parse_result (r : string) : result =
  let results = [("1-0",WhiteWon);("0-1",BlackWon);("1/2-1/2",Draw)] in
  match List.Assoc.find results ~equal:String.equal r with
  | Some r -> r
  | None -> Other

let parse_piece (c : char) : piece option =
  match c with
  | 'R' -> Some Rook
  | 'N' -> Some Knight
  | 'B' -> Some Bishop
  | 'Q' -> Some Queen
  | 'K' -> Some King
  | _ -> None

let parse_rank (c : char) : rank option =
  match c with
  | '1' -> Some R1
  | '2' -> Some R2
  | '3' -> Some R3
  | '4' -> Some R4
  | '5' -> Some R5
  | '6' -> Some R6
  | '7' -> Some R7
  | '8' -> Some R8
  | _ -> None

let parse_file (c : char) : file option =
  match c with
  | 'a' -> Some FA
  | 'b' -> Some FB
  | 'c' -> Some FC
  | 'd' -> Some FD
  | 'e' -> Some FE
  | 'f' -> Some FF
  | 'g' -> Some FG
  | 'h' -> Some FH
  | _ -> None

(* This in particular is horrifying and could be nice with parser combinators,
   or just at lex time with regexps.

   Not supporting quality annotations (e.g., ??, !, etc)
 *)
let parse_normal_move (m : string) : normal_move =
  let cs : char list = String.to_list m in
  let (piece,cs) =
    match cs with
    | (c::cs) ->
       begin
         match parse_piece c with
         | Some p -> (p,cs)
         | None -> (Pawn, (c::cs))
       end
    | _ -> raise (InvalidMove m)
  in
  (* The rest is easier from the back.  From the front, the first location
     might be the destination OR piece disambiguation *)
  let cs = List.rev cs in
  let (check,mate,cs) =
    match cs with
    | '+'::cs -> (true,false,cs)
    | '#'::cs -> (false,true,cs)
    | _ -> (false,false,cs)
  in
  let (promote,cs) =
    match cs with
    | c::'='::cs ->
       begin
         match parse_piece c with
         | Some p -> (Some p,cs)
         | None   -> raise (InvalidMove m)
       end
    | _ -> (None,cs)
  in
  let (toFile,toRank,cs) =
    match cs with
    | cr :: cf :: cs ->
       begin
         match (parse_file cf, parse_rank cr) with
         | (Some file, Some rank) -> (file,rank,cs)
         | _ -> raise (InvalidMove m)
       end
    | _ -> raise (InvalidMove m)
  in
  let (capture,cs) =
    match cs with
    | 'x' :: cs -> (true,cs)
    | _ -> (false,cs)
  in
  let (fromRank,fromFile) =
    match cs with
    | [] -> (None,None)
    | c :: [] ->
       begin
         match parse_file c with
         | Some f -> (None, Some f)
         | None ->
            match parse_rank c with
            | Some r -> (Some r, None)
            | None -> raise (InvalidMove m)
       end
    | cr :: cf :: [] ->
       begin
         match (parse_rank cr, parse_file cf) with
         | (Some r, Some f) -> (Some r, Some f)
         | _ -> raise (InvalidMove m)
       end
    | _ -> raise (InvalidMove m)
  in
  {piece; toRank; toFile; fromRank; fromFile; check; mate; capture; promote}

let parse_castle_move (m : string) : castle_move =
  let cs = List.rev (String.to_list m) in
  let (check,mate,cs) =
    match cs with
    | '+' :: cs -> (true,false,cs)
    | '#' :: cs -> (false,true,cs)
    | _ -> (false,false,cs)
  in
  match cs with
  | ['O';'-';'O'] -> {side=KingSide;check;mate}
  | ['O';'-';'O';'-';'O'] -> {side=QueenSide;check;mate}
  | _ -> raise (InvalidMove m)

let parse_move (m : string) : move =
  if Char.equal 'O' (String.get m 0)
  then Castle (parse_castle_move m)
  else Normal (parse_normal_move m)

let is_move_number (m : string) : bool =
  Char.equal '.' (String.get m (String.length m - 1))

(* Not supporting annotations/commentary for now *)
let parse_moves (moves : string list) (result : string) (result_absent : bool) =
  (* This is filtering out move numbers and checking the last thing to see if
     it's a result *)
  let rec parse_moves_helper (acc : move list) (moves : string list)
          : move list =
    match moves with
    | [] -> List.rev acc
    | (m :: []) when not result_absent ->
       if String.equal m result then List.rev acc else
         raise (InvalidMove (m ^ " (result " ^ result ^ " expected)"))
    | (m :: moves) when is_move_number m ->
       parse_moves_helper acc moves
    | (m :: moves) -> parse_moves_helper (parse_move m :: acc) moves
  in
  parse_moves_helper [] moves


let sanitize tags moves result_unknown =
  let (gi_event,tags) = get_tag "Event" tags in
  let (gi_site,tags) = get_tag "Site" tags in
  let (date_string,tags) = get_tag "Date" tags in
  let gi_date = parse_date date_string in
  let (round_string,tags) = get_tag "Round" tags in
  let gi_round =
    try
      begin
        let r : string list = String.split ~on:'.' round_string in
        List.map ~f:Int.of_string r
      end
    with
    | _ ->
       begin
         if false then
           Printf.printf "Warning: discarding unintelligible round %s\n"
                         round_string
         else ();
         []
       end
  in
  let (gi_white,tags) = get_tag "White" tags in
  let (gi_black,tags) = get_tag "Black" tags in
  let (result_string,tags) = get_tag "Result" tags in
  let gi_result = parse_result result_string in
  let moves = parse_moves moves result_string result_unknown in
  {game_info={gi_event; gi_site; gi_date; gi_round; gi_white; gi_black;
              gi_result};
   other_tags=tags;
   moves = moves}

%}

%token <string> STRING
%token PERIOD
%token ASTERISK
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token <int> NAG
%token <string> SYMBOL
%token <string> COMMENT
%token EOF

%start toks
%type <token list> toks

%start pgn
%type <Pgn.pgn> pgn
%type <string> move
%type <string*string> tag_pair

%start only_pgn
%type <Pgn.pgn> only_pgn

%start pgns
%type <Pgn.pgn list> pgns

%%

(* tok and toks are just parsers that print out the token stream, for
   debugging *)
tok :
| STRING         { STRING $1 }
| PERIOD         { PERIOD }
| ASTERISK       { ASTERISK }
| LEFT_BRACKET   { LEFT_BRACKET }
| RIGHT_BRACKET  { RIGHT_BRACKET }
| LEFT_PAREN     { LEFT_PAREN }
| RIGHT_PAREN    { RIGHT_PAREN }
| LEFT_ANGLE     { LEFT_ANGLE }
| RIGHT_ANGLE    { RIGHT_ANGLE }
| NAG            { NAG $1 }
| SYMBOL         { SYMBOL $1 }
| COMMENT        { COMMENT $1 }
;

toks :
| tok* EOF { $1 }
;


move :
| SYMBOL PERIOD+ SYMBOL  { $3 }
| SYMBOL NAG* { $1 }
;

tag_pair :
| LEFT_BRACKET SYMBOL STRING RIGHT_BRACKET { ($2,$3) }
;

pgn :
| tag_pair+ move* ASTERISK  { sanitize $1 $2 true }
| tag_pair+ move*  { sanitize $1 $2 false }
;

only_pgn :
| pgn EOF { $1 }
;


pgns :
| pgn+ EOF { $1 }
;

%%
