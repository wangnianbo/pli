open Ast
open Format
open Printf
open Str

let get_4spaces = "    "

let rec get_typeStmt stmt =
	match stmt with
	| Primitivestmt(varident, Bool) -> String.concat "" [varident; " : "; "bool"]
	| Primitivestmt(varident, Int) -> String.concat "" [varident; " : "; "int"]
	| Highstmt(varident, customtype) -> String.concat "" [varident; " : "; customtype]
	| Blockstmt(varident, block) -> String.concat "" [varident; " : "; "{"; (analysis block); "}"] 
and analysis block =
	match block with
	| [] -> ""
	| x::tail -> String.concat "" [get_typeStmt x; analysis tail]


let rec get_typeStmts stmts=
	match stmts with
	| [] -> ""
	| x::tail -> if List.length stmts > 1 then String.concat "" [(get_typeStmt x) ;", " ;(get_typeStmts tail)]
											else get_typeStmt x

let get_typeBlock typeblock = 
	match typeblock with
	| Typedef(stmts,typeident) -> String.concat "" ["typedef ";"{";(get_typeStmts stmts);"} ";typeident] (*list of stmt*)
	| TypedefBeanType(Bool,ident) -> String.concat "" ["typedef bool ";ident] (*list of stmt*)
	| TypedefBeanType(Int,ident) -> String.concat "" ["typedef int ";ident] (*list of stmt*)
	| TypedefIdent(ident,ident2) -> String.concat "" ["typedef "; ident ;" ";ident2] (*list of stmt*)
	
	
(*format is typdef list*)



let get_bean_type beanType= 
	match beanType with
	| Bool -> String.concat "" ["bool"]
	| Int -> String.concat "" ["int"]

let get_parameter parameter= 
	match parameter with
  	| ValP(varD , beanType , varName) -> String.concat "" ["val "; (get_bean_type beanType) ;" ";  varName]
  	| RefP(refD , refType , varName) -> String.concat "" ["ref "; refType;" "; varName]
   	| ValBeanP(varD , refType , varName) -> String.concat "" ["val "; refType;" "; varName]
  	| RefBeanP(refD , beanType , varName) -> String.concat "" ["ref "; (get_bean_type beanType);" "; varName]


let rec get_parameters parameters = 
	match parameters with
	| [] -> ""
	| x::[] -> String.concat "" [(get_parameter x)]
	| x::tail -> if List.length parameters > 1 then String.concat "" [(get_parameter x) ; ", " ;(get_parameters tail)]
												else get_parameter x


let get_proc_header procHeader = 
	match procHeader with
	| ProcHeader( procName , parameters ) -> String.concat "" [procName;"(";(get_parameters parameters);")"]



let rec get_lvalue lvalue = 
	match lvalue with
	| LId(ident) -> String.concat "" [ident]
	| LField(ident,lvalue) -> String.concat "" [ident;".";(get_lvalue lvalue)]




let rec get_alExpr expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" [] 
and get_expr_followed_is_plus expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_followed_is_sub expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2);]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2);]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_followed_is_mul expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_followed_is_div expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_pre_is_plus expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_div expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_pre_is_sub expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_pre_is_mul expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_pre_is_div expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| Ebinop (expr1,Op_add,expr2) -> String.concat "" ["(";(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2);")"]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2);")"]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" ["(";(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2);")"]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" ["(";(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2);")"]
	| EunopInAlExpr ( Op_minus, expr2) -> String.concat "" ["-";(get_expr_pre_is_div expr2)]

	| _ -> String.concat "" []
and get_expr_pre_is_minus expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]
	| _ -> String.concat "" ["(";(get_alExpr expr);")"] 





let rec get_logicExpr expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| ElvalInLogicExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop4 (expr1,Op_or,expr2) -> String.concat "" [(get_logicExpr expr1);" or ";(get_logicExpr expr2)]
	| Ebinop4 (expr1,Op_and,expr2) -> String.concat "" [(get_pre_or_followed_is_and expr1);" and ";(get_pre_or_followed_is_and expr2)]

	| EunopInLogicExpr (Op_not,expr2) -> String.concat "" [" not ";(get_logicExpr expr2)]


	| Ebinop1 (expr1,Op_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" = ";(get_pre_is_relation_with_al expr2)]
	| Ebinop1 (expr1,Op_not_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" != ";(get_pre_is_relation_with_al expr2)]

	| Ebinop2 (expr1,Op_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);"  = ";(get_pre_is_relation expr2)]
	| Ebinop2 (expr1,Op_not_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" != ";(get_pre_is_relation expr2)]

	| Ebinop3 (expr1,Op_eq,expr2) -> String.concat "" [(get_followed_is_relation expr1);"  = ";(get_pre_is_relation_with_al expr2)]
	| Ebinop3 (expr1,Op_not_eq,expr2) -> String.concat "" [(get_followed_is_relation expr1);" != ";(get_pre_is_relation_with_al expr2)]

	| Ebinop4 (expr1,Op_eq,expr2) -> String.concat "" [(get_followed_is_relation expr1);"  = ";(get_pre_is_relation expr2)]
	| Ebinop4 (expr1,Op_not_eq,expr2) -> String.concat "" [(get_followed_is_relation expr1);" != ";(get_pre_is_relation expr2)]



	| Ebinop1 (expr1,Op_small_than,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" < ";(get_pre_is_relation_with_al expr2)]
	| Ebinop1 (expr1,Op_small_and_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" <= ";(get_pre_is_relation_with_al expr2)]
	| Ebinop1 (expr1,Op_large,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" > ";(get_pre_is_relation_with_al expr2)]
	| Ebinop1 (expr1,Op_large_and_eq,expr2) -> String.concat "" [(get_followed_is_relation_with_al expr1);" >= ";(get_pre_is_relation_with_al expr2)]

	| _ -> String.concat "" [] 
and get_pre_or_followed_is_and expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| ElvalInLogicExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop4 (expr1,Op_eq,expr2) -> String.concat "" ["(";(get_logicExpr expr1);" or ";(get_logicExpr expr2);")"]
	| _ -> String.concat "" [(get_logicExpr expr)] 

and get_followed_is_relation expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| ElvalInLogicExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop4 (expr1,Op_eq,expr2) -> String.concat "" ["(";(get_logicExpr expr1);" or ";(get_logicExpr expr2);")"]
	| Ebinop4 (expr1,Op_not_eq,expr2) -> String.concat "" ["(";(get_pre_or_followed_is_and expr1);" and ";(get_pre_or_followed_is_and expr2);")"]
	| EunopInLogicExpr (Op_not,expr2) -> String.concat "" ["( not ";(get_logicExpr expr2);")"]
	| _ -> String.concat "" [(get_logicExpr expr)] 
and get_followed_is_relation_with_al expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" [] 
and get_pre_is_relation_with_al expr = 
	match expr with
	| Eint(intVal) -> String.concat "" [(string_of_int intVal)]
	| ElvalInAlExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop (expr1,Op_add,expr2) -> String.concat "" [(get_expr_followed_is_plus expr1);" + ";(get_expr_pre_is_plus expr2)]
	| Ebinop (expr1,Op_sub,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" - ";(get_expr_pre_is_sub expr2)]
	| Ebinop (expr1,Op_mul,expr2) -> String.concat "" [(get_expr_followed_is_mul expr1);" * ";(get_expr_pre_is_mul expr2)]
	| Ebinop (expr1,Op_div,expr2) -> String.concat "" [(get_expr_followed_is_sub expr1);" / ";(get_expr_pre_is_div expr2)]
	| _ -> String.concat "" [] 
and get_pre_is_relation expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| ElvalInLogicExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| _ -> String.concat "" ["(";(get_logicExpr expr);")"] 

and get_pre_is_not expr = 
	match expr with
	| Ebool(boolVal) -> String.concat "" [(string_of_bool boolVal)]
	| ElvalInLogicExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]

	| Ebinop4 (expr1,Op_eq,expr2) -> String.concat "" ["(";(get_logicExpr expr1);" or ";(get_logicExpr expr2);")"]
	| Ebinop4 (expr1,Op_not_eq,expr2) -> String.concat "" ["(";(get_pre_or_followed_is_and expr1);" and ";(get_pre_or_followed_is_and expr2);")"]
	| EunopInLogicExpr (Op_not,expr2) -> String.concat "" ["( not ";(get_logicExpr expr2);")"]
	| _ -> String.concat "" [(get_logicExpr expr)]



let rec get_fieldInitializer fieldInitializer = 
	match fieldInitializer with
	| FieldInitializerForm(ident,rvalue) -> String.concat "" [ident;" = ";(get_rvalue rvalue);""]
and get_fieldInitializers fieldInitializers = 
	match fieldInitializers with
	| [] -> ""
	| x::[] -> String.concat "" [(get_fieldInitializer x)]
	| x::tail -> String.concat "" [(get_fieldInitializer x);", ";(get_fieldInitializers tail);]
and get_structure fieldInitializers = 
	match fieldInitializers with
	| FieldInitializers(fieldInitializers) -> String.concat "" ["{";(get_fieldInitializers fieldInitializers);"}"]
and get_rvalue rvalue = 
	match rvalue with
	| LogicexprInRvalue(logicExpr) -> String.concat "" [(get_logicExpr logicExpr)]
	| AlexprInRvalue(alExpr) -> String.concat "" [(get_alExpr alExpr)]

	| Structure(structure) -> String.concat "" [(get_structure structure)]

(*
let get_stmt stmt = 
	match stmt with
	| Assign(lvalue,rvalue) -> String.concat "" [(get_lvalue lvalue);" := ";(get_rvalue rvalue);";\n"]
*)

let get_expr expr = 
	match expr with
	| LogicexprInExpr(logicExpr) -> String.concat "" [(get_logicExpr logicExpr)]
	| AlexprInExpr(alExpr) -> String.concat "" [(get_alExpr alExpr)]
	| LvalueInExpr(lvalue) -> String.concat "" [(get_lvalue lvalue)]


let rec get_expList exprList= 
	match exprList with
	| [] -> ""
	| x::[] -> String.concat "" [(get_expr x)]
	| x::tail -> String.concat "" [(get_expr x);", ";(get_expList tail)]



let add4Spaces  str =    String.concat "\n    "  [""; String.concat "\n    " ( Str.split (Str.regexp "\n") str )]


let rec get_stmts stmts = 
	match stmts with
	| [] -> ""
	| x::[] -> String.concat "" [(get_stmt x);]
	| x::tail -> String.concat "\n" [(get_stmt x);(get_stmts tail);]
		and  get_stmt x =
					 match x with
					 | Assign(lvalue,rvalue) -> String.concat "" [get_4spaces;(get_lvalue lvalue);" := ";(get_rvalue rvalue);";"]
					 | ReadExpre (varName) ->  String.concat "" [get_4spaces;"read "; varName; ";"]
					 | WriteExpre (stringExpre) ->  String.concat "" [get_4spaces;"write "; stringExpre; ";"]
					 | WriteVar  (expr)  ->  String.concat "" [get_4spaces;"write "; get_expr expr; ";"]
					 | ExprList(ident,exprList) -> String.concat "" [get_4spaces;ident;"("; get_expList exprList; ")";";"]
					 | IfExpre (logicExpr,ifStmts)  ->  String.concat "" [get_4spaces;"if ";(get_logicExpr logicExpr);" then";add4Spaces(get_stmts ifStmts); "\n    fi"]
					 | IfElseExpre (logicExpr,ifElseIfStmts, ifElseElseStmts) -> String.concat "" [get_4spaces;"if ";(get_logicExpr logicExpr);" then"; ( add4Spaces(get_stmts ifElseIfStmts)); "\n    else";  add4Spaces(get_stmts ifElseElseStmts); "\n    fi"]
					 | WhileExpre (logicExpr,whileStmts) ->  String.concat "" [get_4spaces;"while ";(get_logicExpr logicExpr);" do"; add4Spaces(get_stmts whileStmts); "\n    od"]


let get_localVarDecl localVarDecl = 
	match localVarDecl with
	| ValTypeDecl(beanType,varName) -> String.concat "" [get_4spaces;(get_bean_type beanType);" ";varName;";\n"]
	| RefTypeDecl(refType,varName) -> String.concat "" [get_4spaces;refType;" ";varName;";\n"]


let rec get_localVarDecls localVarDecls = 
	match localVarDecls with
	| [] -> ""
	| x::[] -> String.concat "" [(get_localVarDecl x)]
	| x::tail -> String.concat "" [(get_localVarDecl x);(get_localVarDecls tail)]


let rec get_proc_body procBody = 
	match procBody with
	| { localVarDecls = localVarDecls; stmts = stmts } -> String.concat "" [(get_localVarDecls localVarDecls);"\n";(get_stmts stmts)]


let get_proc proc = 
	match proc with
	| Proc( procHeader , procBody ) -> String.concat "" ["\nproc ";(get_proc_header procHeader);"\n";(get_proc_body procBody);"\nend"]

let rec get_procs procs = 
	match procs with
	| [] -> ""
	| x::tail -> String.concat "" [(get_proc x) ; "\n" ;(get_procs tail)]
	
let rec print_typeDef format = 
	match format with
	| [] -> ""
	| x::tail -> String.concat "" [(get_typeBlock x); "\n";(print_typeDef tail)] (*single typedef*)


	
let print_program fmt prog = 
	match prog with
	| {typedefs = typedefs ; procs = procs} -> String.concat "" [(print_typeDef typedefs);(get_procs procs)]
	