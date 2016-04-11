/* File parser.mly */
%{
open Ast
%}
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> STRING_CONST
%token <string> IDENT
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN

%token <char> UNKNOW
%token PLUS MINUS MUL DIV
%token SEMICOLON COMMA STRUCTKEY

%token WHILE DO OD
%token IF THEN FI ELSE

%token OR AND NOT EQUAL BIGGER SMALLER NOEQUAL NOBIGGER NOSMALLER
%token INT BOOL

%token LBRACKET RBRACKET

%token PROC END COLON DOT 

%token VAL REF

%token EOF

%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL NOEQUAL SMALLER NOBIGGER BIGGER NOSMALLER
%left PLUS MINUS        /* lowest precedence */
%left MUL DIV         /* medium precedence */


%start main             /* the entry point */
%type <Ast.program> main
%%
main:
	typedefs procs { {typedefs = List.rev $1; procs = List.rev $2} }    /*   ----------------   here should be rev  */
;

unMatch:
 UNKNOW {}
/*---------Typedef------*/
	
typedefs:
	/*multiple structs*/
	| typedefs typedef  { $2::$1 }					/*   ----------------   change */
	/*single struct*/
	| typedef {$1 :: []}
	| { [] }

typedef:
	/*single struct*/
	STRUCTKEY LBRACKET statements RBRACKET IDENT { Typedef ($3,$5) }
	
				
				

statement:
	| IDENT COLON IDENT          	 { Highstmt ($1, $3) }	
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



alExpr:
  | INT_CONST { Eint $1 }
  | lvalue { ElvalInAlExpr $1 }
  /* Binary operators */
  | alExpr PLUS alExpr { Ebinop ($1, Op_add, $3) }
  | alExpr MINUS alExpr { Ebinop ($1, Op_sub, $3) }
  | alExpr MUL alExpr { Ebinop ($1, Op_mul, $3) }
  | alExpr DIV alExpr { Ebinop ($1, Op_div, $3) }	
  | MINUS alExpr { EunopInAlExpr ( Op_minus, $2) }
  | LPAREN alExpr RPAREN { $2 }



logicExpr:
	| BOOL_CONST { Ebool ($1) }
    | lvalue { ElvalInLogicExpr($1) }

	| logicExpr OR logicExpr { Ebinop4 ($1, Op_or, $3) }
	| logicExpr AND logicExpr { Ebinop4 ($1, Op_and, $3) }
	| NOT logicExpr { EunopInLogicExpr ( Op_not, $2) }


  	| alExpr EQUAL alExpr { Ebinop1 ($1, Op_eq, $3) }
  	| alExpr NOEQUAL alExpr { Ebinop1 ($1, Op_not_eq, $3) }

  	| alExpr EQUAL logicExpr { Ebinop2 ($1, Op_eq, $3) }
  	| alExpr NOEQUAL logicExpr { Ebinop2 ($1, Op_not_eq, $3) }

   	| logicExpr EQUAL alExpr { Ebinop3 ($1, Op_eq, $3) }
  	| logicExpr NOEQUAL alExpr { Ebinop3 ($1, Op_not_eq, $3) }

  	| logicExpr EQUAL logicExpr { Ebinop4 ($1, Op_eq, $3) }
  	| logicExpr NOEQUAL logicExpr { Ebinop4 ($1, Op_not_eq, $3) }

  	| alExpr SMALLER alExpr { Ebinop1 ($1, Op_small_than, $3) }
  	| alExpr NOBIGGER alExpr { Ebinop1 ($1, Op_small_and_eq, $3) }
  	| alExpr BIGGER alExpr { Ebinop1 ($1, Op_large, $3) }
  	| alExpr NOSMALLER alExpr { Ebinop1 ($1, Op_large_and_eq, $3) }


    | LPAREN logicExpr RPAREN { $2 }



fieldInitializer:
	| IDENT EQUAL rvalue { FieldInitializerForm($1,$3) }

fieldInitializers:
	| fieldInitializer COMMA fieldInitializers {$1::$3}
	| fieldInitializer {$1::[]}
	| {[]}

structure:
	| LBRACKET fieldInitializers RBRACKET	{ FieldInitializers($2) }

rvalue:
	| logicExpr {LogicexprInRvalue($1)}
	| alExpr {AlexprInRvalue($1)}
	| structure {Structure($1)}

expr:
	| logicExpr {LogicexprInExpr($1)}
	| alExpr {AlexprInExpr($1)}
	| lvalue {LvalueInExpr $1}

exprList:
	| exprList COMMA expr { $3 :: $1 }
	| expr { $1 :: [] }
	| { [] }


stmt:
	| lvalue ASSIGN rvalue SEMICOLON { Assign($1,$3) }
	| READ IDENT SEMICOLON {ReadExpre $2}
	| WRITE STRING_CONST SEMICOLON {WriteExpre $2}
	| WRITE expr SEMICOLON {WriteVar $2}
	| IDENT LPAREN exprList RPAREN SEMICOLON {ExprList($1,$3)}

	| IF logicExpr THEN stmts FI {IfExpre($2,$4)}
	| IF logicExpr THEN stmts ELSE stmts FI {IfElseExpre ($2,$4,$6)}
	| WHILE logicExpr DO stmts OD {WhileExpre($2,$4)}



stmts:
	| stmts stmt { $2 :: $1 }
	| stmt { $1 :: [] }


localVarDecl:
	| beanType varName SEMICOLON { ValTypeDecl($1,$2) }
	| varName varName SEMICOLON { RefTypeDecl($1,$2) }


localVarDecls:
	| localVarDecls localVarDecl { $2 :: $1 }
	| localVarDecl { $1 :: [] }
	| { [] }

procBody:
	| stmts { { localVarDecls = []; stmts = $1 } }
	| localVarDecls stmts { { localVarDecls = List.rev $1; stmts =  List.rev $2 } }



paremeter:
	| varD beanType varName 	{ValP($1,$2,$3)}
	| refD varName varName 	{RefP($1,$2,$3)}
	| varD varName varName	{ValBeanP($1,$2,$3)}
	| refD beanType varName 	{RefBeanP($1,$2,$3)}



paremeters:
	| paremeter COMMA paremeters { $1 :: $3 }
	| paremeter { $1 :: [] }
	| { [] }

procHeader:
	| procName LPAREN paremeters RPAREN { ProcHeader($1,$3) }


proc:
	| PROC procHeader procBody END	{ Proc ($2 ,$3 ) }


procs:
	| procs proc { $2 :: $1 }
	| proc { $1 :: [] }
	| { [] } 
