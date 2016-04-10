

type procName = string

type varName = string

type refType = string


type beanType = 
  | Bool
  | Int

(*type define 
stmt->typeStmt
varident->varName
lowtype->beanType
typeident->varName
*)


type typeStmt =
	| Primitivestmt of (varName * beanType)
	| Blockstmt of (varName * block)
and block = typeStmt list

type typedefform = 
	| Typedef of (typeStmt list * varName)

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

type parameters = parameter list

type procHeader = ProcHeader of ( procName * parameters ) 

type binop =
  | Op_add 
  | Op_sub 
  | Op_mul 
  | Op_div
  | Op_eq 
  | Op_lt

type unop =
  | Op_minus

type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

type rvalue =
  | Rexpr of expr 

type stmt = 
  | Assign of (lvalue * rvalue)


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
 
