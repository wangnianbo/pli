/* File parser.mly o */
%{
open Ast
%}
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> IDENT
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN

%token PLUS MINUS MUL DIV
%token SEMICOLON COMMA STRUCTKEY

%token WHILE DO OD
%token IF THEN FI

%token OR AND NOT EQUAL BIGGER SMALLER NOEQUAL NOBIGGER NOSMALLER
%token INT BOOL

%token LBRACKET RBRACKET

%token PROC END COLON DOT 

%token VAL REF

%token EOF


%left PLUS MINUS        /* lowest precedence */
%left MUL DIV         /* medium precedence */

%start main             /* the entry point */
%type <Ast.program> main
%%
main:
	typedefs procs { {typedefs= $1; procs = List.rev $2} }
;

/*---------Typedef------*/
	
typedefs:
	/*multiple structs*/
	typedef typedefs { $1::$2 }
	/*single struct*/
	| typedef {$1 :: []}
	| { [] }

typedef:
	/*single struct*/
	STRUCTKEY LBRACKET statements RBRACKET IDENT { Typedef ($3,$5) }
	
				
				

statement:
	| IDENT COLON basictype          { Primitivestmt ($1, $3) }
	| IDENT COLON innertypedef       { Blockstmt ($1, $3) }

statements:
	| statement COMMA statements { $1::$3 }
	| statement {$1::[]}
	| { [] }

basictype:
	| INT {Int}
	| BOOL {Bool}

innertypedef:
			/*STRUCTKEY { expr } r_record*/
  | LBRACKET statements RBRACKET { $2 }
;

/*------------PROCEDURE DEFINE*/

procName:
	| IDENT	{$1}

lvalue:
	| IDENT { LId($1) }
	| IDENT DOT lvalue { LField($1,$3) }


beanType:
	| INT	{Int}
	| BOOL	{Bool}

varD:
	|VAL { Val }

refD:
	|REF { Ref }

varName:
	| IDENT { $1 }


expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  | LPAREN expr RPAREN { $2 }

rvalue:
	| expr {Rexpr($1)}

stmt:
	| lvalue ASSIGN rvalue SEMICOLON { Assign($1,$3) }



stmts:
	| stmt stmts { $1 :: $2 }
	| stmt { $1 :: [] }
	| { [] } 


localVarDecl:
	| beanType varName SEMICOLON { ValTypeDecl($1,$2) }
	| varName varName SEMICOLON { RefTypeDecl($1,$2) }


localVarDecls:
	| localVarDecl { $1 :: [] }
	| { [] }

procBody:
	| localVarDecls stmts { { localVarDecls = $1; stmts = $2 } }


paremeter:
	| varD beanType varName 	{ValP($1,$2,$3)}
	| refD varName varName 	{RefP($1,$2,$3)}


paremeters:
	| paremeter COMMA paremeters { $1 :: $3 }
	| paremeter { $1 :: [] }
	| { [] }

procHeader:
	| procName LPAREN paremeters RPAREN { ProcHeader($1,$3) }


proc:
	| PROC procHeader procBody END	{ Proc ($2 ,$3 ) }


procs:
	| proc procs { $1 :: $2 }
	| proc { $1 :: [] }
	| { [] } 
