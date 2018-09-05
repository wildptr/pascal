open Batteries
open Canon

type env = {
  tab : VarSet.t array;
  procs : proc array
}

let rec scan_stmt env = function
  | C_CallStmt (_, proc, args) ->
    let n = Array.length proc.params in
    for i=0 to n-1 do
      let param = proc.params.(i) in
      if param.byref then
        match args.(i) with
        | C_VarExpr v ->
          let proc_body = env.procs.(proc.id) in
          let lid = proc_body.var_start.(proc.depth) + i in
          let p = proc_body.vars.(lid) in
          assert (p.qual_name = proc.qual_name ^ "." ^ param.name);
          Printf.eprintf "alias: %s -- %s\n" p.qual_name v.qual_name;
          env.tab.(p.gid) <- VarSet.add v env.tab.(p.gid)
        | _ -> failwith "var-bound argument not a variable"
    done
  | _ -> ()

let scan_proc env proc =
  List.iter (scan_stmt env) proc.body

let build_alias_table prog =
  let tab = Array.make (Array.length prog.vars) VarSet.empty in
  let env = { tab; procs = prog.procs } in
  Array.iter (scan_proc env) prog.procs;
  tab
