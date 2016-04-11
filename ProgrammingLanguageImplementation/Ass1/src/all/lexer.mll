(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)

let line_number = ref 1

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
let comment = "#" [^'\n']*

rule token = parse
  | stringReq as stringArg     { !line_number, STRING_CONST stringArg }
  | comment { token lexbuf } 
  | [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '\n'          { incr line_number; Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm {!line_number, INT_CONST(int_of_string lxm) }
  (* keywords *)
  | "write"        {!line_number, WRITE }
  | "read"         {!line_number, READ }

  | "while"        {!line_number, WHILE }
  | "do"           {!line_number, DO }
  | "od"           {!line_number, OD }
  | "if"           {!line_number, IF }
  | "then"         {!line_number, THEN }
  | "else"         {!line_number, ELSE }
  | "fi"           {!line_number, FI } 
  | "or"           {!line_number, OR }
  | "and"          {!line_number, AND }
  | "not"          {!line_number, NOT }
  | "="            {!line_number, EQUAL }
  | ":="           {!line_number, ASSIGN }
  | ">"            {!line_number, BIGGER}
  | "<"            {!line_number, SMALLER }
  | "!="           {!line_number, NOEQUAL }
  | "<="           {!line_number, NOBIGGER }
  | ">="           {!line_number, NOSMALLER }
  | '+'            {!line_number, PLUS }
  | '-'            {!line_number, MINUS }
  | '*'            {!line_number, MUL }
  | ';'            {!line_number, SEMICOLON }
  | ':'			       {!line_number, COLON }
  | "/"            {!line_number, DIV }
  | "("            {!line_number, LPAREN }
  | ")"            {!line_number, RPAREN }
  | "int"          {!line_number, INT }
  | "bool"         {!line_number, BOOL }
  | "true"         {!line_number, BOOL_CONST true }
  | "false"        {!line_number, BOOL_CONST false }
  | "proc"         {!line_number, PROC }
  | "val"          {!line_number, VAL }
  | "ref"          {!line_number, REF }
  | "end"          {!line_number, END }
  | ","            {!line_number, COMMA }
  | structKey      {!line_number, STRUCTKEY }
  | "."            {!line_number, DOT }
  | "{"            {!line_number, LBRACKET }
  | "}"            {!line_number, RBRACKET }
  | ident as lxm {!line_number, IDENT lxm }
  | eof            {!line_number, EOF }
  | _             { token lexbuf }            
	






