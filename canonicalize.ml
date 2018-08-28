open Batteries
open Ast
open Canon

module M = Map.String

type entry =
  | Var of var
  | Proc of proc

type canon_env = {
  symtab: entry M.t;
  mutable stmts : stmt list;
}

let emit env s =
  env.stmts <- s :: env.stmts

let resolve_type = function
  | "integer" -> IntType
  | "boolean" -> BoolType
  | name -> failwith ("unknown type: "^name)

let canon_ident_expr symtab name =
  match M.find name symtab with
  | Var v -> v
  | _ -> failwith ("not a variable: " ^ name)

let canon_lvalue env = function
  | A_IdentExpr name -> canon_ident_expr env.symtab name
  | _ -> failwith "not an l-value"

let rec canon_expr env = function
  | A_IntExpr i -> C_IntExpr i
  | A_BoolExpr b -> C_BoolExpr b
  | A_IdentExpr name -> C_VarExpr (canon_ident_expr env.symtab name)
  | A_BinaryExpr (op, e1, e2) ->
    let e1' = canon_expr env e1 in
    let e2' = canon_expr env e2 in
    C_BinaryExpr (op, e1', e2')

let rec canon_stmt env = function
  | A_CompStmt l -> l |> List.iter (canon_stmt env)
  | A_AssignStmt (lhs, rhs) ->
    let lhs' = canon_lvalue env lhs in
    let rhs' = canon_expr env rhs in
    emit env (C_AssignStmt (lhs', rhs'))
  | A_AssertStmt e ->
    emit env (C_AssertStmt (canon_expr env e))
  | A_IfStmt (cond, bodyT, bodyF) ->
    let cond' = canon_expr env cond in
    let envT = { env with stmts = [] } in
    canon_stmt envT bodyT;
    let bodyT' = List.rev envT.stmts in
    let envF = { env with stmts = [] } in
    canon_stmt envF bodyF;
    let bodyF' = List.rev envF.stmts in
    emit env (C_IfStmt (cond', bodyT', bodyF'))
  | A_RepeatStmt (inv, body, cond) ->
    let inv' =
      match inv with
      | Some inv -> canon_expr env inv
      | None -> C_BoolExpr true
    in
    let env' = { env with stmts = [] } in
    body |> List.iter (canon_stmt env');
    let cond' = canon_expr env' cond in
    let body' = List.rev env'.stmts in
    emit env (C_RepeatStmt (inv', body', cond'))

type proc_tree = ProcTree of proc * proc_tree list

let rec canon_proc parent_symtab first_var_id (proc : ast_proc) =
  let symtab, var_id, vars_rev =
    proc.block.vars |> List.fold_left begin fun (st, id, vr) (names, ast_type) ->
      let typ = resolve_type ast_type in
      names |> List.fold_left begin fun (st, id, vr) name ->
        let v = { name; typ; id } in
        M.add name (Var v) st, id+1, v :: vr
      end (st, id, vr)
    end (parent_symtab, first_var_id, [])
  in
  let vars = vars_rev |> List.rev |> Array.of_list in
  let symtab, subprocs_rev =
    proc.block.procs |> List.fold_left begin fun (st, subs) ast_proc ->
      let ProcTree (subproc, _) as subproc_tree =
        canon_proc st var_id ast_proc
      in
      let st' = M.add ast_proc.name (Proc subproc) st in
      let subs' = subproc_tree :: subs in
      st', subs'
    end (symtab, [])
  in
  let env = { symtab; stmts = [] } in
  proc.block.body |> List.iter (canon_stmt env);
  ProcTree
    ({ name = proc.name; arg_types = [||]; ret_types = [||];
       body = env.stmts |> List.rev; vars }, List.rev subprocs_rev)

let rec flatten_proc_tree t =
  let acc = ref [] in
  let rec visit (ProcTree (p, l)) =
    l |> List.iter visit;
    acc := p :: !acc
  in
  visit t;
  List.rev !acc

let canon_program (prog : ast_program) =
  let (proc : ast_proc) = { name = prog.name; block = prog.block } in
  canon_proc M.empty 0 proc |> flatten_proc_tree
