{
open Pgn_parse

module String = Core.String
module Int = Core.Int

exception LexError of string

let err (c : char) (l : Lexing.lexbuf) =
  raise (LexError
    (Printf.sprintf
       "Unexpected character while lexing: %c (line: %d, col: %d)\n"
       c Lexing.(l.lex_curr_p.pos_lnum) Lexing.(l.lex_curr_p.pos_cnum)))


let token_to_string (t : token) : string =
  match t with
  | STRING s       -> "STRING " ^ s
  | PERIOD         -> "PERIOD"
  | ASTERISK       -> "ASTERISK"
  | LEFT_BRACKET   -> "LEFT_BRACKET"
  | RIGHT_BRACKET  -> "RIGHT_BRACKET"
  | LEFT_PAREN     -> "LEFT_PAREN"
  | RIGHT_PAREN    -> "RIGHT_PAREN"
  | LEFT_ANGLE     -> "LEFT_ANGLE"
  | RIGHT_ANGLE    -> "RIGHT_ANGLE"
  | NAG i          -> "NAG " ^ (Int.to_string i)
  | SYMBOL s       -> "SYMBOL " ^ s
  | COMMENT s      -> "COMMENT " ^ s
  | EOF            -> "EOF"

}

let white = [' ' '\t' '\r' '\n']+
let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let alphanum = letter | digit
(* The spec doesn't include '/' as a symbol char, but this is obviously wrong
   because it later says the move list is ended with a game termination marker,
   which is called a symbol, and one of the examples of which is 1/2-1/2 *)
let symbol_char = alphanum | ['_' '+' '#' '=' ':' '-' '/']
let symbol = alphanum symbol_char*

(* The way we're parsing strings is wrong according to the PGN spec.
   The '"' char is supposed to be escapable with '\'.  But the big
   PGN file I have has a bunch of strings that end in '\', so to
   parse them I have to not follow the rules.  Which is good, anyway,
   since disallowing escape characters makes my life easier. Even worse,
   there are weird non-printable characters in the strings (unicode,
   maybe?)  So I'm just allowing everything except '"'. *)
let string_char = [^ '"']

rule read =
  parse
  | white                                     { read lexbuf }
  | '"' (string_char* as s) '"'               { STRING s }
  (* I'm just dropping these comments to make my life easier in the lexer.
     They aren't supposed to show up in my sample, but they do.
  | '{' ([^ '}']* as c) '}'                   { COMMENT c }
  | ';' ([^ '\r' '\n']* as c) ['\r' '\n']*    { COMMENT c } *)
  | '{' ([^ '}']*) '}'                   { read lexbuf }
  | ';' ([^ '\r' '\n']*) ['\r' '\n']*    { read lexbuf }
  | '.'                                       { PERIOD }
  | '*'                                       { ASTERISK }
  | '['                                       { LEFT_BRACKET }
  | ']'                                       { RIGHT_BRACKET }
  | '('                                       { LEFT_PAREN }
  | ')'                                       { RIGHT_PAREN }
  | '<'                                       { LEFT_ANGLE }
  | '>'                                       { RIGHT_ANGLE }
  | '$' (digit+ as d)                         { NAG (Int.of_string d) }
  | symbol as s                               { SYMBOL s }
  (* UTF-8 BOM.  If I were less lazy I'd look for it just at the start *)
  | '\xEF' '\xBB' '\xBF'                      { read lexbuf }
  | _ as c                                    { err c lexbuf }
  | eof                                       { EOF }

