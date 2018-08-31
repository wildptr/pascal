open Batteries
open Canon

module S = Set.Int

let rec scan_stmt tab = function
  | C_CallStmt (_, proc, args) ->
    let n = Array.length proc.params in
    for i=0 to n-1 do
      let param = proc.params.(i) in
      if param.byref then
        match args.(i) with
        | C_VarExpr v ->
          Printf.eprintf "alias: %s.%s -- %s\n"
            proc.qual_name param.name v.qual_name;
          tab.(i) <- S.add v.gid tab.(i)
        | _ -> failwith "var-bound argument not a variable"
    done
  | _ -> ()

let scan_proc tab proc =
  List.iter (scan_stmt tab) proc.body

let build_alias_table prog =
  let tab = Array.make (Array.length prog.vars) S.empty in
  Array.iter (scan_proc tab) prog.procs;
  tab
