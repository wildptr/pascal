open Batteries
open Canon

type env = {
  tab : bool array;
  proc_id : int;
}

let rec scan_expr env = function
  | C_IntExpr _ | C_BoolExpr _ -> ()
  | C_VarExpr v ->
    if v.proc_id <> env.proc_id then
      env.tab.(v.gid) <- true
  | C_UnaryExpr (_, e, _) -> scan_expr env e
  | C_BinaryExpr (_, e1, e2, _) -> scan_expr env e1; scan_expr env e2

let rec scan_stmt env = function
  | C_AssignStmt (v, e) ->
    if v.proc_id <> env.proc_id then
      env.tab.(v.gid) <- true;
    scan_expr env e
  | C_AssertStmt _ | C_AssumeStmt _ -> ()
  | C_IfStmt (e, s1, s2) ->
    scan_expr env e;
    List.iter (scan_stmt env) s1;
    List.iter (scan_stmt env) s2
  | C_RepeatStmt (_, s, e) ->
    List.iter (scan_stmt env) s;
    scan_expr env e
  | C_CallStmt (vars, proc, args) ->
    vars |> Array.iter begin fun (v:var) ->
      if v.proc_id <> env.proc_id then
        env.tab.(v.gid) <- true
    end;
    args |> Array.iteri begin fun i e ->
      if proc.params.(i).byref then
        let [@warning "-8"] C_VarExpr v = e in
        env.tab.(v.gid) <- true
      else scan_expr env e
    end

let scan_proc tab proc =
  let env = { tab; proc_id = proc.head.id } in
  proc.body |> List.iter (scan_stmt env)

let build_table prog =
  let nv = Array.length prog.vars in
  let tab = Array.make nv false in
  prog.procs |> Array.iter (scan_proc tab);
  for i=0 to nv-1 do
    if tab.(i) then
      Printf.eprintf "visible: %s\n" prog.vars.(i).qual_name
  done;
  tab
