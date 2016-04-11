

type procName = string

type varName = string
type stringExpre = string
type refType = string

type ident = string


type beanType = 
  | Bool
  | Int


type hightype = string
(*type define 
stmt->typeStmt
varident->varName
lowtype->beanType
typeident->varName
*)


type typeStmt =
	| Primitivestmt of (varName * beanType)
  | Highstmt of (varName * hightype)
	| Blockstmt of (varName * block)
and block = typeStmt list

type typedefform = 
	| Typedef of (typeStmt list * varName)
	| TypedefBeanType of (beanType * varName)
	| TypedefIdent of (varName * varName)


type format = typedefform list 

(*procedure defin*)

type lvalue =
  | LId of varName
  | LField of ( varName * lvalue )

type varD = Val
type refD = Ref

type parameter = 
  | ValP of (varD * beanType * varName)
  | RefP of (refD * refType * varName)
  | ValBeanP of (varD * refType * varName)
  | RefBeanP of (refD * beanType * varName)


type parameters = parameter list

type procHeader = ProcHeader of ( procName * parameters ) 

type binop =
  | Op_or
  | Op_and
  | Op_eq
  | Op_not_eq
  | Op_small_than
  | Op_small_and_eq
  | Op_large
  | Op_large_and_eq
  | Op_add 
  | Op_sub 
  | Op_mul 
  | Op_div

type unop =
  | Op_not
  | Op_minus

type alExpr =
  | Eint of int
  | ElvalInAlExpr of lvalue
  | Ebinop of (alExpr * binop * alExpr)
  | EunopInAlExpr of (unop * alExpr)

type logicExpr = 
  | Ebool of bool
  | ElvalInLogicExpr of lvalue
  | Ebinop1 of (alExpr * binop * alExpr)
  | Ebinop2 of (alExpr * binop * logicExpr)
  | Ebinop3 of (logicExpr * binop * alExpr)
  | Ebinop4 of (logicExpr * binop * logicExpr)
  | EunopInLogicExpr of (unop * logicExpr)


type rvalue =
  | LogicexprInRvalue of logicExpr 
  | AlexprInRvalue of alExpr
  | Structure of structure
and fieldInitializer = 
  | FieldInitializerForm of ( ident * rvalue )
and structure = 
  | FieldInitializers of fieldInitializer list


type expr = 
  | LogicexprInExpr of logicExpr 
  | AlexprInExpr of alExpr
  | LvalueInExpr of lvalue


type exprList = expr list


type stmt = 
  | Assign of (lvalue * rvalue)
  | ReadExpre of (varName)
  | WriteExpre of (stringExpre)
  | WriteVar of (expr)
  | ExprList of (ident * exprList)
  | IfExpre of ( logicExpr * ifStmts)
  | IfElseExpre of (logicExpr * ifStmts * elseStmts)
  | WhileExpre of (logicExpr * iwStmts)
 and
 iwStmts = stmt list	
 and
 ifStmts = stmt list
 and
 elseStmts = stmt list

type localVarDecl = 
  | ValTypeDecl of (beanType * varName)
  | RefTypeDecl of (refType * varName)



type procBody = {

  localVarDecls: localVarDecl list;
  stmts: stmt list

}


type proc = Proc of ( procHeader * procBody )

type program = {
  typedefs: typedefform list;
  procs : proc list
}
 
