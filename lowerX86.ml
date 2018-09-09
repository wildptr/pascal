open Batteries
open Ir
open X86

module X86_IR = MakeIR(MakeInst(X86))

type lower_env = {
  mutable next_reg : int;
  mutable insts : inst list
}

let fresh_reg env =
  let i = env.next_reg in
  env.next_reg <- i+1;
  i

let flush env =
  let insts = List.rev env.insts in
  env.insts <- [];
  insts

let emit env inst =
  env.insts <- inst :: env.insts

let to_reg env = function
  | Reg r -> r
  | e ->
    let r = fresh_reg env in
    MOV (r, e) |> emit env;
    r

let rec lower env = function
  | I_Reg r -> Reg r
  | I_Imm i -> Imm i
  | I_Label s -> Label s
  | I_Unary (op, e) ->
    let e' = lower env e |> to_reg env in
    begin match op with
      | I_NOT | I_NEG ->
        let op' =
          match op with
          | I_NOT -> NOT
          | I_NEG -> NEG
          | _ -> assert false
        in
        let r = fresh_reg env in
        UNARY (op', r, e') |> emit env;
        Reg r
      | I_Z | I_NZ ->
        let cond =
          match op with
          | I_Z -> Z
          | I_NZ -> NZ
          | _ -> assert false
        in
        let r = fresh_reg env in
        SET (Cond1 (cond, e'), r) |> emit env;
        Reg r
    end
  | I_Binary (op, e1, e2) ->
    let e1' = lower env e1 |> to_reg env in
    let e2' = lower env e2 in
    begin match op with
      | I_ADD | I_SUB | I_MUL | I_AND | I_OR | I_XOR ->
        let op' =
          match op with
          | I_ADD -> ADD
          | I_SUB -> SUB
          | I_MUL -> MUL
          | I_AND -> AND
          | I_OR  -> OR
          | I_XOR -> XOR
          | _ -> assert false
        in
        let r = fresh_reg env in
        BINARY (op', r, e1', e2') |> emit env;
        Reg r
      | I_EQ | I_NE | I_LT | I_GE | I_GT | I_LE ->
        let cond =
          match op with
          | I_EQ -> EQ
          | I_NE -> NE
          | I_LT -> LT
          | I_GE -> GE
          | I_GT -> GT
          | I_LE -> LE
          | _ -> assert false
        in
        let r = fresh_reg env in
        SET (Cond2 (cond, e1', e2'), r) |> emit env;
        Reg r
    end
  | I_Load (size, addr) ->
    let m = lower_mem env size addr in
    let r = fresh_reg env in
    LOAD (r, m) |> emit env;
    Reg r

and lower_mem env size = function
  | I_Binary (I_ADD, I_Reg r, I_Imm i) ->
    { base = Some r; index = None; disp = i; size }
  | I_Binary (I_ADD, I_Imm i, I_Reg r) ->
    { base = Some r; index = None; disp = i; size }
  | e ->
    begin match lower env e with
      | Reg r -> { base = Some r; index = None; disp = 0; size }
      | Imm i -> { base = None; index = None; disp = i; size }
      | Label s -> failwith "lower_mem: Label"
    end

let rec lower_cond env = function
  | I_Unary (I_NOT, I_Unary (I_Z|I_NZ as op, e)) ->
    let op_neg =
      match op with
      | I_Z -> I_NZ
      | I_NZ -> I_Z
      | _ -> assert false
    in
    lower_cond env (I_Unary (op_neg, e))
  | I_Unary (I_NOT, I_Binary (I_EQ|I_NE|I_LT|I_GE|I_GT|I_LE as op, e1, e2)) ->
    let op_neg =
      match op with
      | I_EQ -> I_NE
      | I_NE -> I_EQ
      | I_LT -> I_GE
      | I_GE -> I_LT
      | I_GT -> I_LE
      | I_LE -> I_GT
      | _ -> assert false
    in
    lower_cond env (I_Binary (op_neg, e1, e2))
  | I_Unary (I_Z|I_NZ as op, e) ->
    let e' = lower env e |> to_reg env in
    let c =
      match op with
      | I_Z -> Z
      | I_NZ -> NZ
      | _ -> assert false
    in
    Cond1 (c, e')
  | I_Binary (I_EQ|I_NE|I_LT|I_GE|I_GT|I_LE as op, e1, e2) ->
    let e1' = lower env e1 |> to_reg env in
    let e2' = lower env e2 in
    let c =
      match op with
      | I_EQ -> EQ
      | I_NE -> NE
      | I_LT -> LT
      | I_GE -> GE
      | I_GT -> GT
      | I_LE -> LE
      | _ -> assert false
    in
    Cond2 (c, e1', e2')
  | e ->
    let e' = lower env e |> to_reg env in
    Cond1 (NZ, e')

let lower_inst env = function
  | I_Set (r, e) ->
    let e' = lower env e in
    MOV (r, e') |> emit env
  | I_Store (size, addr, e) ->
    let m = lower_mem env size addr in
    let e' = lower env e in
    STORE (m, e') |> emit env
  | I_Jump e ->
    let e' = lower env e in
    JMP e' |> emit env
  | I_Branch (cond, e) ->
    let cond' = lower_cond env cond in
    let e' = lower env e in
    CJMP (cond', e') |> emit env
  | I_Call (e, l) ->
    let e' = lower env e in
    List.iter (fun r -> PUSH (Reg r) |> emit env) l;
    CALL e' |> emit env
  | I_Return ->
    RET |> emit env
  | I_Phi (r, rhs) ->
    PHI (r, rhs) |> emit env

let lower_proc (p : abs_proc) =
  let env = { next_reg = p.n_reg; insts = [] } in
  let blocks =
    p.blocks |> Array.map begin fun (b : abs_block) ->
      b.insts |> List.iter (lower_inst env);
      let insts = flush env in
      X86_IR.{ insts; succ = b.succ; name = b.name }
    end
  in
  X86_IR.{ name = p.name; blocks; n_reg = env.next_reg }
