{
open Batteries
open Lexing
open Parser

exception Error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1 }

}

let int = ('0' | ['1'-'9'] ['0'-'9']*)
let white = [' ' '\t']+
let newline = '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read = parse
  | white               { read lexbuf }
  | newline             { next_line lexbuf; read lexbuf }
  | int                 { Int (int_of_string (lexeme lexbuf)) }
  | ":="                { ColonEq }
  | "<="                { LtEq }
  | "<>"                { NotEq }
  | ">="                { GtEq }
  | "("                 { LParen }
  | ")"                 { RParen }
  | "*"                 { Star }
  | "+"                 { Plus }
  | ","                 { Comma }
  | "-"                 { Minus }
  | "."                 { Dot }
  | ":"                 { Colon }
  | ";"                 { Semi }
  | "<"                 { Lt }
  | "="                 { Eq }
  | ">"                 { Gt }
  | "and"               { AND }
  | "array"             { ARRAY }
  | "assert"            { ASSERT }
  | "begin"             { BEGIN }
  | "case"              { CASE }
  | "const"             { CONST }
  | "div"               { DIV }
  | "do"                { DO }
  | "downto"            { DOWNTO }
  | "else"              { ELSE }
  | "end"               { END }
  | "false"             { FALSE }
  | "for"               { FOR }
  | "function"          { FUNCTION }
  | "if"                { IF }
  | "in"                { IN }
  | "mod"               { MOD }
  | "nil"               { NIL }
  | "not"               { NOT }
  | "of"                { OF }
  | "or"                { OR }
  | "packed"            { PACKED }
  | "procedure"         { PROCEDURE }
  | "program"           { PROGRAM }
  | "record"            { RECORD }
  | "repeat"            { REPEAT }
  | "set"               { SET }
  | "shl"               { SHL }
  | "shr"               { SHR }
  | "then"              { THEN }
  | "to"                { TO }
  | "true"              { TRUE }
  | "type"              { TYPE }
  | "until"             { UNTIL }
  | "var"               { VAR }
  | "while"             { WHILE }
  | "xor"               { XOR }
  | id                  { Ident (lexeme lexbuf) }
  | eof                 { EOF }
