open Batteries
open Canon

module G = Graph.Imperative.Graph.Concrete(Var)
module Comp = Graph.Components.Undirected(G)

type env = {
  graph : G.t;
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
          G.add_edge env.graph p v
        | _ -> failwith "var-bound argument not a variable"
    done
  | _ -> ()

let scan_proc env proc =
  List.iter (scan_stmt env) proc.body

let build_alias_tab prog =
  let nv = Array.length prog.vars in
  let graph = G.create ~size:nv () in
  for i=0 to nv-1 do
    G.add_vertex graph prog.vars.(i)
  done;
  let env = { graph; procs = prog.procs } in
  Array.iter (scan_proc env) prog.procs;
  let tab = Array.make nv [] in
  Comp.components_list graph |> List.iter
    (fun clist ->
       clist |> List.iter (fun c -> tab.(c.gid) <- List.remove clist c));
  for i=0 to nv-1 do
    tab.(i) |> List.iter
      (fun (v:var) ->
         Printf.eprintf "alias: %s -- %s\n"
           prog.vars.(i).qual_name v.qual_name)
  done;
  tab
