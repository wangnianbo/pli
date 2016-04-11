
(* Procedure Name *)
type procName = string

(*Variable Name*)
type varName = string

(* String Expression *)
type stringExpre = string

(* reference type *)
type refType = string

(* idnetifier name *)
type ident = string

(* Bean type bool and int *)
type beanType = 
  | Bool
  | Int

(* iner type *)
type hightype = string

(*type define 
stmt->typeStmt
varident->varName
lowtype->beanType
typeident->varName
*)

(* statement in type define *)
type typeStmt =
	| Primitivestmt of (varName * beanType)
  | Highstmt of (varName * hightype)
	| Blockstmt of (varName * block)
and block = typeStmt list

(* defination of type defination*)
type typedefform = 
	| Typedef of (typeStmt list * varName)
	| TypedefBeanType of (beanType * varName)
	| TypedefIdent of (varName * varName)

(* root defination of type defination *)
type format = typedefform list 

(*procedure define *)

type lvalue =
  | LId of varName
  | LField of ( varName * lvalue )

type varD = Val
type refD = Ref

(* Parameter *)
type parameter = 
  | ValP of (varD * beanType * varName)
  | RefP of (refD * refType * varName)
  | ValBeanP of (varD * refType * varName)
  | RefBeanP of (refD * beanType * varName)

(* Parameter list *)
type parameters = parameter list

(* procedure header *)
type procHeader = ProcHeader of ( procName * parameters ) 

(* Binary operation *)
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

(* unary operation *)
type unop =
  | Op_not
  | Op_minus

(* algebraic expression *)
type alExpr =
  | Eint of int
  | ElvalInAlExpr of lvalue
  | Ebinop of (alExpr * binop * alExpr)
  | EunopInAlExpr of (unop * alExpr)

(*simple logical expression *)
type simpleLogicExpr =
  | ElvalInSimpleLogicExpr of lvalue
  | EunopInSimpleLogicExpr of ( unop * lvalue)

(* logical expression *)
type logicExpr = 
  | Ebool of bool
  | ElvalInLogicExpr of lvalue
  | Ebinop1 of (alExpr * binop * alExpr)
  | Ebinop2 of (alExpr * binop * logicExpr)
  | Ebinop3 of (logicExpr * binop * alExpr)
  | Ebinop4 of (logicExpr * binop * logicExpr)
  | EunopInLogicExpr of (unop * logicExpr)

(* right value *)
type rvalue =
  | LogicexprInRvalue of logicExpr 
  | AlexprInRvalue of alExpr
  | Structure of structure
and fieldInitializer = 
  | FieldInitializerForm of ( ident * rvalue )
and structure = 
  | FieldInitializers of fieldInitializer list

(* general expression *)
type expr = 
  | LogicexprInExpr of logicExpr 
  | AlexprInExpr of alExpr
  | LvalueInExpr of lvalue

(* general expression list *)
type exprList = expr list

(* stmt expression *)
type stmt = 
  | Assign of (lvalue * rvalue)
  | ReadExpre of (varName)
  | WriteExpre of (stringExpre)
  | WriteVar of (expr)
  | ExprList of (ident * exprList)
  | IfExpreInSimple of (simpleLogicExpr * ifStmts)
  | IfExpre of ( logicExpr * ifStmts)
  | IfElseExpreInSimple of (simpleLogicExpr * ifStmts * elseStmts)
  | IfElseExpre of (logicExpr * ifStmts * elseStmts)
  | WhileExpreInSimple of (simpleLogicExpr * iwStmts)
  | WhileExpre of (logicExpr * iwStmts)
 and
 iwStmts = stmt list	
 and
 ifStmts = stmt list
 and
 elseStmts = stmt list

 (* logcal var decal *)
type localVarDecl = 
  | ValTypeDecl of (beanType * varName)
  | RefTypeDecl of (refType * varName)

(* procedure expression *)
type procBody = {

  localVarDecls: localVarDecl list;
  stmts: stmt list

}

(* procedure expression *)
type proc = Proc of ( procHeader * procBody )

(* program defination *)
type program = {
  typedefs: typedefform list;
  procs : proc list
}
 
