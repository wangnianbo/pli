(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit | '_'
let digits = digit+
let ident = ('_' | alpha) alnum*
let structKey = "typedef"

let stringExc = [^'\"'] | [^'\t']
let stringReq = '\"' stringExc * '\"'

rule token = parse
  | [' ' '\t' '\n']    { token lexbuf }     (* skip blanks *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }
  | stringReq as stringArg     { STRING_CONST stringArg }
  (* keywords *)
  | "write"        { WRITE }
  | "read"         { READ }

  | "while"        { WHILE }
  | "do"           { DO }
  | "od"           { OD }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "fi"           { FI } 
  | "or"           { OR }
  | "and"          { AND }
  | "not"          { NOT }
  | "="            { EQUAL }
  | ":="           { ASSIGN }
  | ">"            { BIGGER}
  | "<"            { SMALLER }
  | "!="           { NOEQUAL }
  | "<="           { NOBIGGER }
  | ">="           { NOSMALLER }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { MUL }
  | ';'            { SEMICOLON }
  | ':'			   { COLON }
  | "/"            { DIV }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "int"          { INT }
  | "bool"         { BOOL }
  | "true"         { BOOL_CONST true }
  | "false"        { BOOL_CONST false }
  | "proc"         { PROC }
  | "val"          { VAL }
  | "ref"          { REF }
  | "end"          { END }
  | ","            { COMMA }
  | structKey      { STRUCTKEY }
  | "."            { DOT }
  | "{"            { LBRACKET }
  | "}"            { RBRACKET }
  | ident as lxm { IDENT lxm }
  | eof            { EOF }
  | _             { token lexbuf }            
	






