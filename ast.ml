type ast_type =
  | A_IdentType of string
  | A_ArrayType of int * ast_type

type var_decl = string list * ast_type

type param_decl = bool * string list * ast_type

type binary_op =
  | A_Add
  | A_Sub
  | A_Mul
  | A_And
  | A_Or
  | A_Eq | A_NotEq
  | A_Lt | A_GtEq
  | A_Gt | A_LtEq

type ast_expr =
  | A_IntExpr of int
  | A_BoolExpr of bool
  | A_IdentExpr of string
  | A_BinaryExpr of binary_op * ast_expr * ast_expr
  | A_IndexExpr of ast_expr * ast_expr

type ast_stmt =
  | A_CompStmt of ast_stmt list
  | A_AssignStmt of ast_expr * ast_expr
  | A_AssertStmt of ast_expr
  | A_IfStmt of ast_expr * ast_stmt * ast_stmt
  | A_RepeatStmt of ast_expr option * ast_stmt list * ast_expr
  | A_CallStmt of string * ast_expr list

type ast_block = {
  vars : var_decl list;
  procs : ast_proc list;
  body : ast_stmt list;
}

and ast_proc = {
  name : string;
  params : param_decl list;
  block : ast_block;
}

type ast_program = {
  name : string;
  block : ast_block;
}
