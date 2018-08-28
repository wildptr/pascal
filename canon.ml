open Batteries

type typ =
  | IntType
  | BoolType

type var = {
  name : string;
  typ : typ;
  id : int;
}

type binary_op =
  | Add | Sub | Mul
  | Eq | NotEq

type expr =
  | C_IntExpr of int
  | C_VarExpr of var
  | C_BinaryExpr of binary_op * expr * expr

type stmt =
  | C_AssignStmt of var * expr
  | C_AssertStmt of expr
  | C_IfStmt of expr * stmt list * stmt list

type proc = {
  name : string;
  arg_types : typ array;
  ret_types : typ array;
  body : stmt list;
  vars : var array;
}

open Format

let string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Eq -> "="
  | NotEq -> "<>"

let rec pp_expr f = function
  | C_IntExpr i -> pp_print_int f i
  | C_VarExpr var -> pp_print_string f var.name
  | C_BinaryExpr (op, e1, e2) ->
    fprintf f "(%a %s %a)" pp_expr e1 (string_of_binary_op op) pp_expr e2

let pp_indent f indent =
  String.make (indent*2) ' ' |> pp_print_string f

let rec pp_stmt indent f = function
  | C_AssignStmt (lhs, rhs) ->
    fprintf f "%a%s := %a\n" pp_indent indent lhs.name pp_expr rhs
  | C_AssertStmt e ->
    fprintf f "%aassert %a\n" pp_indent indent pp_expr e
  | C_IfStmt (cond, bodyT, bodyF) ->
    fprintf f "%aif %a then begin\n" pp_indent indent pp_expr cond;
    bodyT |> List.iter (pp_stmt (indent+1) f);
    fprintf f "%aend" pp_indent indent;
    if bodyF <> [] then begin
      fprintf f " else begin\n";
      bodyF |> List.iter (pp_stmt (indent+1) f);
    fprintf f "%aend" pp_indent indent;
    end;
    pp_print_char f '\n'

let pp_proc f proc =
  fprintf f "procedure %s\n" proc.name;
  pp_print_string f "begin\n";
  proc.body |> List.iter (pp_stmt 1 f);
  pp_print_string f "end\n";
