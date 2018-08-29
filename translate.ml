open Batteries
open Canon
open Ir
open Abstract

type tblock = {
  mutable insts : inst list;
  name : string;
  id : int;
  mutable succ : int list;
}

let add_succ b1 b2 =
  b1.succ <- b2.id :: b1.succ

let finalize_block blk : block =
  let insts = List.rev blk.insts in
  { insts; succ = blk.succ; name = blk.name }

type var_info = {
  reg : reg;
}

type env = {
  mutable next_reg : int;
  mutable current_block : tblock;
  mutable blocks : tblock list;
  mutable n_block : int;
  var_tab : var_info array;
}

let emit env inst =
  env.current_block.insts <- inst :: env.current_block.insts

let fresh_reg env =
  let i = env.next_reg in
  env.next_reg <- env.next_reg + 1;
  i

let to_reg env = function
  | Reg r -> r
  | o ->
    let r = fresh_reg env in
    emit env (MOV (r, o));
    r

let fresh_block env =
  let i = env.n_block in
  env.n_block <- env.n_block + 1;
  let name = Printf.sprintf ".L%d" i in
  let (blk:tblock) = { insts = []; name; id = i; succ = [] } in
  env.blocks <- blk :: env.blocks;
  blk

let rec translate_expr env = function
  | C_IntExpr i -> mk_imm_i i
  | C_BoolExpr b -> mk_imm_i (if b then 1 else 0)
  | C_VarExpr v -> Reg env.var_tab.(v.id).reg
  | C_UnaryExpr (op, e) ->
    let s = translate_expr env e |> to_reg env in
    let d = fresh_reg env in
    let op' =
      match op with
      | Not -> NOT
    in
    emit env (ALU1 (op', s, d)); Reg d
  | C_BinaryExpr (op, e1, e2) ->
    let s1 = translate_expr env e1 |> to_reg env in
    let s2 = translate_expr env e2 in
    let d = fresh_reg env in
    let inst =
      match op with
      | Add -> ALU2 (ADD, d, s1, s2)
      | Sub -> ALU2 (SUB, d, s1, s2)
      | Mul -> MUL (d, s1, s2)
      | Eq    -> SET (Cond2 (EQ, s1, s2), d)
      | NotEq -> SET (Cond2 (NE, s1, s2), d)
      | Lt    -> SET (Cond2 (LT, s1, s2), d)
      | GtEq  -> SET (Cond2 (GE, s1, s2), d)
      | Gt    -> SET (Cond2 (GT, s1, s2), d)
      | LtEq  -> SET (Cond2 (LE, s1, s2), d)
      | _ -> failwith "not implemented"
    in
    emit env inst; Reg d

let translate_cond_generic env neg target e =
  let r = translate_expr env e |> to_reg env in
  let cond = Cond1 ((if neg then Z else NZ), r) in
  BRANCH (cond, target) |> emit env

let negate = function
  | EQ -> NE
  | NE -> EQ
  | LT -> GE
  | GE -> LT
  | GT -> LE
  | LE -> GT

let translate_cond env neg target = function
  | C_BinaryExpr (op, e1, e2) as e ->
    let c =
      match op with
      | Eq    -> Some EQ
      | NotEq -> Some NE
      | Lt    -> Some LT
      | GtEq  -> Some GE
      | Gt    -> Some GT
      | LtEq  -> Some LT
      | _ -> None
    in
    begin match c with
      | Some c ->
        let s1 = translate_expr env e1 |> to_reg env in
        let s2 = translate_expr env e2 in
        let cond = Cond2 ((if neg then negate c else c), s1, s2) in
        BRANCH (cond, target) |> emit env
      | None -> translate_cond_generic env neg target e
    end
  | e -> translate_cond_generic env neg target e

let translate_lvalue env (v:var) =
  env.var_tab.(v.id).reg

let rec translate_stmt env = function
  | C_AssignStmt (lhs, rhs) ->
    let s = translate_expr env rhs in
    let d = translate_lvalue env lhs in
    emit env (MOV (d, s))
  | C_AssertStmt _ | C_AssumeStmt _ -> ()
  | C_IfStmt (cond, bodyT, bodyF) ->
    let fork = env.current_block in
    let blkT = fresh_block env in
    let blkF = fresh_block env in
    let join = fresh_block env in

    add_succ fork blkF;
    add_succ fork blkT;
    add_succ blkT join;
    add_succ blkF join;

    translate_cond env true ((mk_imm_s blkF.name)) cond;

    env.current_block <- blkT;
    bodyT |> List.iter (translate_stmt env);
    emit env (JUMP ((mk_imm_s join.name)));

    env.current_block <- blkF;
    bodyF |> List.iter (translate_stmt env);

    env.current_block <- join
  | C_RepeatStmt (_, body, cond) ->
    let blk = fresh_block env in
    let target = (mk_imm_s blk.name) in
    env.current_block <- blk;
    body |> List.iter (translate_stmt env);
    translate_cond env true target cond;
    env.current_block <- fresh_block env
  | C_CallStmt (vars, proc, args) ->
    args |> Array.iteri begin fun i arg ->
      let o = translate_expr env arg in
      emit env (MACH (ARG (i, o)))
    end;
    emit env (CALL ((mk_imm_s proc.name)));
    vars |> Array.iteri begin fun i v ->
      let d = translate_lvalue env v in
      emit env (MACH (RETVAL (i, d)))
    end

let translate_proc proc =
  let blk = { insts = []; name = ".L0"; id = 0; succ = [] } in
  let n_var = Array.length proc.vars in
  let var_tab = Array.make n_var { reg = 0 } in
  let env = {
    next_reg = 0;
    current_block = blk;
    blocks = [blk];
    n_block = 1;
    var_tab
  } in
  for i=0 to n_var-1 do
    let reg = fresh_reg env in
    env.var_tab.(i) <- { reg }
  done;
  proc.body |> List.iter (translate_stmt env);
  let blocks =
    env.blocks |> List.map finalize_block |> List.rev |> Array.of_list
  in
  let n_reg = env.next_reg in
  { blocks; n_reg }
