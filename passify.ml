open Canon
open Passive

type env = {
  mutable stmts : passive_stmt list;
}

let emit env s =
  env.stmts <- s :: env.stmts

let rec passify_stmt env = function
  | C_AssignStmt (lhs, rhs) ->
    emit env (P_Assume (C_BinaryExpr (Eq, C_VarExpr lhs, rhs)))
  | C_AssertStmt e ->
    emit env (P_Assert e)
  | C_AssumeStmt e ->
    emit env (P_Assume e)
  | C_IfStmt (cond, bodyT, bodyF) ->
    let envT = { stmts = [] } in
    bodyT |> List.iter (passify_stmt envT);
    let bodyT' = List.rev envT.stmts in
    let envF = { stmts = [] } in
    bodyF |> List.iter (passify_stmt envF);
    let bodyF' = List.rev envF.stmts in
    emit env (P_If (cond, bodyT', bodyF'))
  | C_RepeatStmt _ -> failwith "REPEAT statement"

let passify_proc (proc : proc) =
  let env = { stmts = [] } in
  proc.body |> List.iter (passify_stmt env);
  { body = List.rev env.stmts; vars = proc.vars }
