open Batteries
open Ast
open Canon

module M = Map.String

type entry =
  | Var of var
  | Proc of proc_head

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

let lookup_var symtab name =
  match M.find name symtab with
  | Var v -> v
  | _ -> failwith ("not a variable: " ^ name)

let lookup_proc symtab name =
  match M.find name symtab with
  | Proc p -> p
  | _ -> failwith ("not a procedure: " ^ name)

let canon_lvalue env = function
  | A_IdentExpr name -> lookup_var env.symtab name
  | _ -> failwith "not an l-value"

let check_type e t =
  if type_of_expr e <> t then
    failwith "type error"

let rec canon_expr env = function
  | A_IntExpr i -> C_IntExpr i
  | A_BoolExpr b -> C_BoolExpr b
  | A_IdentExpr name -> C_VarExpr (lookup_var env.symtab name)
  | A_BinaryExpr (op, e1, e2) ->
    let e1' = canon_expr env e1 in
    let e2' = canon_expr env e2 in
    let op', typ =
      match op with
      | A_Add ->
        check_type e1' IntType;
        check_type e2' IntType;
        Add, IntType
      | A_Sub ->
        check_type e1' IntType;
        check_type e2' IntType;
        Sub, IntType
      | A_Mul ->
        check_type e1' IntType;
        check_type e2' IntType;
        Mul, IntType
      | A_And ->
        begin match type_of_expr e1' with
          | IntType ->
            check_type e2' IntType;
            LogAnd, IntType
          | BoolType ->
            check_type e2' BoolType;
            And, BoolType
        end
      | A_Or ->
        begin match type_of_expr e1' with
          | IntType ->
            check_type e2' IntType;
            LogOr, IntType
          | BoolType ->
            check_type e2' BoolType;
            Or, BoolType
        end
      | A_Eq ->
        check_type e2' (type_of_expr e1');
        Eq, BoolType
      | A_NotEq ->
        check_type e2' (type_of_expr e1');
        NotEq, BoolType
      | A_Lt ->
        check_type e1' IntType;
        check_type e2' IntType;
        Lt, BoolType
      | A_GtEq ->
        check_type e1' IntType;
        check_type e2' IntType;
        GtEq, BoolType
      | A_Gt ->
        check_type e1' IntType;
        check_type e2' IntType;
        Gt, BoolType
      | A_LtEq ->
        check_type e1' IntType;
        check_type e2' IntType;
        LtEq, BoolType
    in
    C_BinaryExpr (op', e1', e2', typ)

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
  | A_CallStmt (name, args) ->
    let proc = lookup_proc env.symtab name in
    let args' = args |> List.map (canon_expr env) |> Array.of_list in
    emit env (C_CallStmt ([||], proc, args'))

type canon_proc_env = {
  symtab : entry M.t;
  all_vars : var list;
  var_gid : int;
  proc_id : int;
  qual_name_prefix : string;
  proc_vars : var list;
  var_start : int list;
  var_lid : int;
  depth : int;
  parent_proc : proc_head option
}

type 'a tree = Tree of 'a * 'a tree list

let make_proc_head env (proc : ast_proc) =
  let params =
    proc.params |> List.map begin fun (byref, names, ast_type) ->
      let typ = resolve_type ast_type in
      names |> List.map (fun name -> { byref; name; typ })
    end |> List.concat |> Array.of_list
  in
  let name = proc.name in
  let qual_name = env.qual_name_prefix ^ name in
  let id = env.proc_id in
  let depth = env.depth in
  let head = { name; qual_name; params; id; depth; parent = env.parent_proc } in
  Printf.eprintf "procedure %s(%d) depth: %d\n" name id depth;
  let symtab = M.add name (Proc head) env.symtab in
  { env with symtab; proc_id = id+1 }, head

let rec canon_proc env (proc : ast_proc) =
  let env, head = make_proc_head env proc in
  let params =
    head.params |> Array.to_list |> List.mapi
      (fun i { byref; name; typ } -> name, typ, byref, Some i)
  in
  let local_vars =
    proc.block.vars |> List.map begin fun (names, ast_type) ->
      let typ = resolve_type ast_type in
      names |> List.map (fun name -> name, typ, false, None)
    end |> List.concat
  in
  let symtab, all_vars, var_gid, proc_vars, var_lid =
    List.fold_left begin fun (st, vl, gid, pvl, lid) (name, typ, isref, param_id) ->
      let qual_name = head.qual_name ^ "." ^ name in
      let v =
        { name; qual_name; typ; gid; lid; isref; param_id; proc_id = head.id }
      in
      M.add name (Var v) st, v::vl, gid+1, v::pvl, lid+1
    end (env.symtab, env.all_vars, env.var_gid, env.proc_vars, env.var_lid)
      (params @ local_vars)
  in

  let var_start = env.var_lid :: env.var_start in

  let env', sub_trees_rev =
    let env =
      { symtab;
        all_vars;
        var_gid;
        proc_id = env.proc_id;
        qual_name_prefix = head.qual_name ^ ".";
        proc_vars;
        var_start = env.var_lid :: env.var_start;
        var_lid;
        depth = env.depth+1;
        parent_proc = Some head }
    in
    List.fold_left begin fun (env, l) sub ->
      let env', sub_tree = canon_proc env sub in
      env', sub_tree :: l
    end (env, []) proc.block.procs
  in

  let vars = proc_vars |> List.rev |> Array.of_list in
  let canon_env = { symtab = env'.symtab; stmts = [] } in
  proc.block.body |> List.iter (canon_stmt canon_env);
  let proc =
    { head;
      body = List.rev canon_env.stmts;
      vars;
      var_start = var_start |> List.rev |> Array.of_list }
  in
  { env with
    all_vars = env'.all_vars; var_gid = env'.var_gid; proc_id = env'.proc_id },
  Tree (proc, List.rev sub_trees_rev)

let flatten_tree t =
  let acc = ref [] in
  let rec visit (Tree (n, c)) =
    acc := n :: !acc;
    List.iter visit c
  in
  visit t;
  List.rev !acc

let canon_program (prog : ast_program) =
  let (proc : ast_proc) =
    { name = prog.name; params = []; block = prog.block }
  in
  let env = {
    symtab = M.empty;
    all_vars = [];
    var_gid = 0;
    proc_id = 0;
    qual_name_prefix = "";
    proc_vars = [];
    var_start = [];
    var_lid = 0;
    depth = 0;
    parent_proc = None
  } in
  let env', proc_tree = canon_proc env proc in
  let procs = flatten_tree proc_tree |> Array.of_list in
  for i=0 to env'.proc_id-1 do
    assert (procs.(i).head.id = i)
  done;
  { procs; vars = env'.all_vars |> List.rev |> Array.of_list }
