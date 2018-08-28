type ast_type = string

type var_decl = string list * ast_type

type ast_expr =
  | A_IntExpr of int
  | A_BoolExpr of bool
  | A_IdentExpr of string
  | A_BinaryExpr of Canon.binary_op * ast_expr * ast_expr

(*
type ast_log_expr =
  | A_IntLogExpr of int
  | A_IdentLogExpr of string
  | A_BinaryLogExpr of Canon.binary_op * ast_log_expr * ast_log_expr
*)

type ast_stmt =
  | A_CompStmt of ast_stmt list
  | A_AssignStmt of ast_expr * ast_expr
  | A_AssertStmt of ast_expr
  | A_IfStmt of ast_expr * ast_stmt * ast_stmt
  | A_RepeatStmt of ast_expr option * ast_stmt list * ast_expr

type ast_block = {
  vars : var_decl list;
  procs : ast_proc list;
  body : ast_stmt list;
}

and ast_proc = {
  name : string;
  block : ast_block;
}

type ast_program = {
  name : string;
  block : ast_block;
}
