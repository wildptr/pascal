open Batteries
open Canon

type var_info = {
  cur : var;
  ver : int;
}

type var_info_persist = {
  orig : var;
  cache : (int, var) Hashtbl.t;
}

type env = {
  mutable tab_stack : var_info array list;
  ptab : var_info_persist array;
  mutable new_vars : var list;
  mutable var_id : int;
  n_old_var : int;
}

let top env =
  List.hd env.tab_stack

let push env =
  let new_tab = Array.copy (top env) in
  env.tab_stack <- new_tab :: env.tab_stack

let pop env =
  let tab = top env in
  env.tab_stack <- List.tl env.tab_stack;
  tab

let new_version env id =
  let tab = List.hd env.tab_stack in
  let vi = tab.(id) in
  let ver = vi.ver + 1 in
  let orig = env.ptab.(id).orig in
  let id' = env.var_id in
  env.var_id <- env.var_id + 1;
  let cur =
    { name = Printf.sprintf "%s#%d" orig.name ver; typ = orig.typ; id = id'}
  in
  env.new_vars <- cur :: env.new_vars;
  tab.(id) <- { cur; ver };
  Hashtbl.add env.ptab.(id).cache ver cur;
  cur

let fresh_var env id =
  let tab = List.hd env.tab_stack in
  let ver = tab.(id).ver + 1 in
  match Hashtbl.find env.ptab.(id).cache ver with
  | exception Not_found -> new_version env id
  | v ->
    let tab = top env in
    tab.(id) <- { cur = v; ver };
    v

let rec dsa_expr tab = function
  | C_IntExpr _ as e -> e
  | C_VarExpr v -> C_VarExpr tab.(v.id).cur
  | C_BinaryExpr (op, e1, e2) ->
    let e1' = dsa_expr tab e1 in
    let e2' = dsa_expr tab e2 in
    C_BinaryExpr (op, e1', e2')

let rec dsa_stmt env = function
  | C_AssignStmt (lhs, rhs) ->
    let rhs' = dsa_expr (top env) rhs in
    let lhs' = fresh_var env lhs.id in
    C_AssignStmt (lhs', rhs')
  | C_AssertStmt e ->
    C_AssertStmt (dsa_expr (top env) e)
  | C_IfStmt (cond, bodyT, bodyF) ->
    let cond' = dsa_expr (top env) cond in
    push env;
    let bodyT' = bodyT |> List.map (dsa_stmt env) in
    let tabT = pop env in
    push env;
    let bodyF' = bodyF |> List.map (dsa_stmt env) in
    let tabF = pop env in
    let assignsT = ref [] in
    let assignsF = ref [] in
    let tab = top env in
    for i=0 to env.n_old_var-1 do
      let verT = tabT.(i).ver in
      let verF = tabF.(i).ver in
      let vi =
        if verT < verF then begin
          let assign = C_AssignStmt (tabF.(i).cur, C_VarExpr tabT.(i).cur) in
          assignsT := assign :: !assignsT;
          tabF.(i)
        end else if verF < verT then begin
          let assign = C_AssignStmt (tabT.(i).cur, C_VarExpr tabF.(i).cur) in
          assignsF := assign :: !assignsF;
          tabT.(i)
        end else tabT.(i)
      in
      tab.(i) <- vi
    done;
    let bodyT' = bodyT' @ (List.rev !assignsT) in
    let bodyF' = bodyF' @ (List.rev !assignsF) in
    C_IfStmt (cond', bodyT', bodyF')

let dsa_proc proc =
  let n_old_var = Array.length proc.vars in
  let tab = proc.vars |> Array.map (fun v -> { cur = v; ver = 0 }) in
  let ptab =
    proc.vars |> Array.map (fun v -> { orig = v; cache = Hashtbl.create 0 })
  in
  let env =
    { tab_stack = [tab]; ptab; new_vars = []; var_id = n_old_var; n_old_var }
  in
  let body = proc.body |> List.map (dsa_stmt env) in
  let new_vars = env.new_vars |> List.rev |> Array.of_list in
  let n_new_var = Array.length new_vars in
  let vars =
    if n_old_var > 0 then
      let vars = Array.make env.var_id proc.vars.(0) in
      Array.blit proc.vars 0 vars 0 n_old_var;
      Array.blit new_vars 0 vars n_old_var n_new_var;
      vars
    else [||]
  in
  { proc with body; vars }
