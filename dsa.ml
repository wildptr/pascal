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

type stack_entry = {
  tab : var_info array;
  mutable stmts : stmt list;
}

type global_env = {
  mutable var_id : int;
  mutable new_vars : var list;
  alias_tab : VarSet.t array
}

type env = {
  mutable stack : stack_entry list;
  ptab : var_info_persist array;
  mutable var_id : int;
  mutable new_vars : var list;
  n_old_var : int;
  global : global_env;
  alias_tab : int list array
}

let top_tab env =
  (List.hd env.stack).tab

let push env =
  let tab = Array.copy (top_tab env) in
  env.stack <- { tab; stmts = [] } :: env.stack

let pop env =
  match env.stack with
  | { tab; stmts } :: tl -> env.stack <- tl; tab, List.rev stmts
  | _ -> failwith "pop"

let new_var env f =
  let genv = env.global in
  let lid = env.var_id in
  env.var_id <- lid+1;
  let gid = genv.var_id in
  genv.var_id <- gid+1;
  let v = f lid gid in
  genv.new_vars <- v :: genv.new_vars;
  env.new_vars <- v :: env.new_vars;
  v

let new_version env id =
  let tab = top_tab env in
  let vi = tab.(id) in
  let ver = vi.ver + 1 in
  let orig = env.ptab.(id).orig in
  let cur =
    let suffix = "#" ^ string_of_int ver in
    new_var env
      (fun lid gid ->
         { orig with
           name = orig.name ^ suffix;
           qual_name = orig.qual_name ^ suffix;
           lid;
           gid })
  in
  tab.(id) <- { cur; ver };
  Hashtbl.add env.ptab.(id).cache ver cur;
  cur

let fresh_var env id =
  let tab = top_tab env in
  let ver = tab.(id).ver + 1 in
  match Hashtbl.find env.ptab.(id).cache ver with
  | exception Not_found -> new_version env id
  | v ->
    let tab = top_tab env in
    tab.(id) <- { cur = v; ver };
    v

let fresh_var_alias env id =
  let v = fresh_var env id in
  env.alias_tab.(id) |> List.iter begin fun alias_id ->
    fresh_var env alias_id |> ignore
  end;
  v

let rec dsa_expr tab = function
  | C_IntExpr _ | C_BoolExpr _ as e -> e
  | C_VarExpr v -> C_VarExpr tab.(v.lid).cur
  | C_UnaryExpr (op, e, typ) -> C_UnaryExpr (op, dsa_expr tab e, typ)
  | C_BinaryExpr (op, e1, e2, typ) ->
    let e1' = dsa_expr tab e1 in
    let e2' = dsa_expr tab e2 in
    C_BinaryExpr (op, e1', e2', typ)

(*
let rec mark_uses tab = function
  | C_IntExpr _ | C_BoolExpr _ -> ()
  | C_VarExpr v -> tab.(v.id) <- true
  | C_BinaryExpr (_, e1, e2) -> mark_uses tab e1; mark_uses tab e2

let rec mark_uses_stmt tab = function
  | C_AssignStmt (_, e) -> mark_uses tab e
  | C_AssertStmt e -> mark_uses tab e
  | C_IfStmt (e, s1, s2) ->
    mark_uses tab e;
    List.iter (mark_uses_stmt tab) s1;
    List.iter (mark_uses_stmt tab) s2
  | C_RepeatStmt (inv, body, cond) ->
    mark_uses tab inv;
    List.iter (mark_uses_stmt tab) body;
    mark_uses tab cond
*)

let emit env s =
  let top = List.hd env.stack in
  top.stmts <- s :: top.stmts

let rec dsa_stmt env = function
  | C_AssignStmt (lhs, rhs) ->
    let rhs' = dsa_expr (top_tab env) rhs in
    let lhs' = fresh_var_alias env lhs.lid in
    C_AssignStmt (lhs', rhs') |> emit env
  | C_AssertStmt e ->
    C_AssertStmt (dsa_expr (top_tab env) e) |> emit env
  | C_AssumeStmt e ->
    C_AssumeStmt (dsa_expr (top_tab env) e) |> emit env
  | C_IfStmt (cond, bodyT, bodyF) ->
    let cond' = dsa_expr (top_tab env) cond in
    push env;
    bodyT |> List.iter (dsa_stmt env);
    let tabT, bodyT' = pop env in
    push env;
    bodyF |> List.iter (dsa_stmt env);
    let tabF, bodyF' = pop env in
    let assignsT = ref [] in
    let assignsF = ref [] in
    let tab = top_tab env in
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
    C_IfStmt (cond', bodyT', bodyF') |> emit env
  | C_RepeatStmt (inv, body, cond) ->
    let tab = top_tab env in
    C_AssertStmt (dsa_expr tab inv) |> emit env;
    let defs =
      body |> List.fold_left begin fun set stmt ->
        match stmt with
        | C_AssignStmt (lhs, _) -> Set.Int.add lhs.lid set
        | _ -> set
      end Set.Int.empty
    in
    defs |> Set.Int.iter (fun i -> fresh_var env i |> ignore);
    C_AssumeStmt (dsa_expr tab inv) |> emit env;
    List.iter (dsa_stmt env) body;
    let cond' = dsa_expr tab cond in
    C_AssertStmt (C_BinaryExpr (Imp, C_UnaryExpr (Not, cond', BoolType), dsa_expr tab inv, BoolType)) |> emit env;
    C_AssumeStmt cond' |> emit env
  | C_CallStmt (vars, proc, args) ->
    let tab = top_tab env in
    let args' = args |> Array.map (dsa_expr tab) in
    let vars' = vars |> Array.map (fun (v:var) -> fresh_var_alias env v.lid) in
    C_CallStmt (vars', proc, args') |> emit env

let dsa_proc (genv : global_env) (proc : proc) =
  let n_old_var = Array.length proc.vars in
  let tab = proc.vars |> Array.map (fun v -> { cur = v; ver = 0 }) in
  let ptab =
    proc.vars |> Array.map (fun v -> { orig = v; cache = Hashtbl.create 0 })
  in
  let alias_tab =
    Array.map
      (fun aliases ->
         aliases |> VarSet.enum |> List.of_enum |> List.map (fun v -> v.lid))
      genv.alias_tab
  in
  let env =
    { stack = [{ tab; stmts=[] }];
      ptab;
      new_vars = [];
      var_id = n_old_var;
      n_old_var;
      global = genv;
      alias_tab }
  in
  proc.body |> List.iter (dsa_stmt env);
  let _, body = pop env in
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

let dsa alias_tab prog =
  let genv =
    { var_id = Array.length prog.vars;
      new_vars = [];
      alias_tab }
  in
  let procs = prog.procs |> Array.map (dsa_proc genv) in
  let vars =
    Array.concat [prog.vars; genv.new_vars |> List.rev |> Array.of_list]
  in
  { procs; vars }
