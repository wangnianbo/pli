open Ast
open Format
open Printf

 let rec get_typeStmt stmt =
	match stmt with
	| Primitivestmt(varident, Bool) -> String.concat "" [varident; ":"; "bool"]
	| Primitivestmt(varident, Int) -> String.concat "" [varident; ":"; "int"]
	| Blockstmt(varident, block) -> String.concat "" [varident; ":"; "{"; (analysis block); "}"] 
and analysis block =
	match block with
	| [] -> ""
	| x::tail -> String.concat "" [get_typeStmt x; analysis tail]


let rec get_typeStmts stmts=
	match stmts with
	| [] -> ""
	| x::tail -> String.concat "" [(get_typeStmt x) ; (get_typeStmts tail)]

let get_typeBlock typeblock = 
	match typeblock with
	| Typedef(stmts,typeident) -> String.concat "" ["typedef";"{";(get_typeStmts stmts);"}";typeident] (*list of stmt*)
	| _ -> "nothing in typeblock" 

(*format is typdef list*)





let get_bean_type beanType= 
	match beanType with
	| Bool -> String.concat "" ["bool "]
	| Int -> String.concat "" ["int "]

let get_parameter parameter= 
	match parameter with
  	| ValP(varD , beanType , varName) -> String.concat "" ["val "; (get_bean_type beanType) ; varName]
  	| RefP(refD , refType , varName) -> String.concat "" ["ref "; refType; varName]

let rec get_parameters parameters = 
	match parameters with
	| [] -> ""
	| x::[] -> String.concat "" [(get_parameter x)]
	| x::tail -> String.concat "" [(get_parameter x) ; "," ;(get_parameters tail)]


let get_proc_header procHeader = 
	match procHeader with
	| ProcHeader( procName , parameters ) -> String.concat "" [procName;"(";(get_parameters parameters);")"]



let rec get_lvalue lvalue = 
	match lvalue with
	| LId(ident) -> String.concat "" [ident]
	| LField(ident,lvalue) -> String.concat "" [ident;".";(get_lvalue lvalue)]



let rec get_expr_followed_is_plus expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_followed_is_sub expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2);]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2);]
	| _ -> String.concat "" []
and get_expr_followed_is_mul expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_followed_is_div expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_pre_is_plus expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_pre_is_sub expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_pre_is_mul expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []
and get_expr_pre_is_div expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" ["(";(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2);")"]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2);")"]
	| _ -> String.concat "" []
let rec get_expr expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| Elval(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" []


let get_rvalue rvalue = 
	match rvalue with
	| Rexpr(expr) -> String.concat "" [(get_expr expr)]
(*
let rec get_stmt stmt = 
	match stmt with
	| Assign(lvalue,rvalue) -> String.concat "" [(get_lvalue lvalue);" := ";(get_rvalue rvalue);";\n"]
	| ReadExpre (varName) ->  String.concat "" ["read "; varName; ";\n"]
	| WriteExpre (stringExpre) ->  String.concat "" ["write "; stringExpre; ";\n"]
	| IfExpre (ifStmts)  ->  String.concat "" ["if then"; get_ifStmts ifStmts; "fi\n"]
and 
*)

let rec get_stmts stmts = 
	match stmts with
	| [] -> ""
	| x::[] -> String.concat "" [(get_stmt x);";\n"]
	| x::tail -> String.concat "" [(get_stmt x);(get_stmts tail);";\n"]
		and  get_stmt x =
					 match x with
					 | Assign(lvalue,rvalue) -> String.concat "" [(get_lvalue lvalue);" := ";(get_rvalue rvalue);";\n"]
					 | ReadExpre (varName) ->  String.concat "" ["read "; varName; ";\n"]
					 | WriteExpre (stringExpre) ->  String.concat "" ["write "; stringExpre; ";\n"]
					 | IfExpre (ifStmts)  ->  String.concat "" ["if then"; get_stmts ifStmts; "fi\n"]

let get_localVarDecl localVarDecl = 
	match localVarDecl with
	| ValTypeDecl(beanType,varName) -> String.concat "" [(get_bean_type beanType);" ";varName]
	| RefTypeDecl(refType,varName) -> String.concat "" [refType;" ";varName]


let rec get_localVarDecls localVarDecls = 
	match localVarDecls with
	| [] -> ""
	| x::[] -> String.concat "" [(get_localVarDecl x)]
	| x::tail -> String.concat "" [(get_localVarDecl x);(get_localVarDecls tail)]


let rec get_proc_body procBody = 
	match procBody with
	| { localVarDecls = localVarDecls; stmts = stmts } -> String.concat "" [(get_localVarDecls localVarDecls);(get_stmts stmts)]


let get_proc proc = 
	match proc with
	| Proc( procHeader , procBody ) -> String.concat "" ["proc ";(get_proc_header procHeader);"\n";(get_proc_body procBody);"\n";"end"]

let rec get_procs procs = 
	match procs with
	| [] -> ""
	| x::tail -> String.concat "" [(get_proc x) ; "\n" ;(get_procs tail)]
	
let rec print_typeDef fm format = 
	match format with
	| [] -> ""
	| x::tail -> String.concat "" [get_typeBlock x; "\n"; print_typeDef fm tail] (*single typedef*)


	
let print_program fmt prog = 
	match prog with
	| {typedefs = typedefs ; procs = procs} -> String.concat "" [(print_typeDef Format.std_formatter typedefs); String.concat "" [(get_procs procs)]]
	