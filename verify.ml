open Batteries
open Canon
open Passive

type env = {
  z3 : Z3.context;
  tab : Z3.Expr.expr array;
}

let rec convert_expr env = function
  | C_IntExpr i -> Z3.Arithmetic.Integer.mk_numeral_i env.z3 i
  | C_BoolExpr b -> Z3.Boolean.mk_val env.z3 b
  | C_VarExpr v -> env.tab.(v.lid)
  | C_UnaryExpr (op, e, _) ->
    let e' = convert_expr env e in
    begin match op with
      | Not -> Z3.Boolean.mk_not env.z3 e'
    end
  | C_BinaryExpr (op, e1, e2, _) ->
    let e1' = convert_expr env e1 in
    let e2' = convert_expr env e2 in
    begin match op with
      | Add -> Z3.Arithmetic.mk_add env.z3 [e1';e2']
      | Sub -> Z3.Arithmetic.mk_sub env.z3 [e1';e2']
      | Mul -> Z3.Arithmetic.mk_sub env.z3 [e1';e2']
      | Eq -> Z3.Boolean.mk_eq env.z3 e1' e2'
      | NotEq -> Z3.Boolean.mk_not env.z3 (Z3.Boolean.mk_eq env.z3 e1' e2')
      | Lt -> Z3.Arithmetic.mk_lt env.z3 e1' e2'
      | GtEq -> Z3.Arithmetic.mk_ge env.z3 e1' e2'
      | Gt -> Z3.Arithmetic.mk_gt env.z3 e1' e2'
      | LtEq -> Z3.Arithmetic.mk_le env.z3 e1' e2'
      | Imp -> Z3.Boolean.mk_implies env.z3 e1' e2'
      | And -> Z3.Boolean.mk_and env.z3 [e1';e2']
      | Or -> Z3.Boolean.mk_or env.z3 [e1';e2']
      | _ -> failwith "not implemented"
    end

let rec wp env s q =
  let open Z3.Boolean in
  match s with
  | P_Assume e ->
    mk_implies env.z3 (convert_expr env e) q
  | P_Assert e ->
    mk_and env.z3 [convert_expr env e; q]
  | P_If (cond, bodyT, bodyF) ->
    let pT = List.fold_right (wp env) bodyT q in
    let pF = List.fold_right (wp env) bodyF q in
    let e = convert_expr env cond in
    let z3 = env.z3 in
    mk_and z3 [mk_implies z3 e pT; mk_implies z3 (mk_not z3 e) pF]

type sat_status = Invalid | Valid | Unknown

let verify_proc proc =
  let z3 = Z3.mk_context [] in
  let int_sort = Z3.Arithmetic.Integer.mk_sort z3 in
  let bool_sort = Z3.Boolean.mk_sort z3 in
  let tab =
    Array.init (Array.length proc.vars) begin fun i ->
      let var = proc.vars.(i) in
      let sort =
        match var.typ with
        | IntType -> int_sort
        | BoolType -> bool_sort
      in
      Z3.Expr.mk_const_s z3 var.name sort
    end
  in
  let env = { z3; tab } in
  let post = Z3.Boolean.mk_val z3 true in
  let vc = List.fold_right (wp env) proc.body post in
  vc |> Z3.Expr.to_string |> print_endline;
  let solver = Z3.Solver.mk_simple_solver z3 in
  Z3.Solver.add solver [Z3.Boolean.mk_not z3 vc];
  let status = Z3.Solver.check solver [] in
  match status with
  | Z3.Solver.UNSATISFIABLE -> Valid
  | Z3.Solver.UNKNOWN -> Unknown
  | Z3.Solver.SATISFIABLE -> Invalid
