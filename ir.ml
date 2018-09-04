open Batteries

type reg = int

type imm = { s : string; i : int }

type opd =
  | Reg of reg
  | Imm of imm

let imm_s s = { s; i = 0 }
let imm_i i = { s = ""; i }
let imm s i = { s; i }

let imm_opd_s s = Imm (imm_s s)
let imm_opd_i i = Imm (imm_i i)
let imm_opd s i = Imm (imm s i)

type alu1 =
  | NOT
  | NEG

type alu2 =
  | ADD
  | SUB
  | AND
  | OR
  | XOR

type cond1 = Z | NZ
type cond2 = EQ | NE | LT | GE | GT | LE

type cond =
  | Cond1 of cond1 * reg
  | Cond2 of cond2 * reg * opd

type mem = reg option * imm

let map_opd f = function
  | Reg r -> Reg (f r)
  | o -> o

let map_mem f (base, offset) =
  Option.map f base, offset

let map_cond f = function
  | Cond1 (op, r) ->
    Cond1 (op, f r)
  | Cond2 (op, r, o) ->
    Cond2 (op, f r, map_opd f o)

module type SpecInstType = sig
  type t
  val map : (reg -> reg) -> t -> t
end

type phi_rhs = { pred : int; mutable r : int }

module OfInstType (I : SpecInstType) = struct

  type inst =
    | MOV of reg * opd
    | ALU1 of alu1 * reg * reg
    | ALU2 of alu2 * reg * reg * opd
    | JUMP of opd
    | BRANCH of cond * opd
    | SET of cond * reg
    | LOAD of reg * mem
    | STORE of mem * opd
    | MUL of reg * reg * opd
    | CALL of opd
    | RET
    | PHI of reg * phi_rhs list
    | MACH of I.t

  let map_inst f = function
    | MOV (r, o) ->
      MOV (f r, map_opd f o)
    | ALU1 (op, r1, r2) ->
      ALU1 (op, f r1, f r2)
    | ALU2 (op, r1, r2, o) ->
      ALU2 (op, f r1, f r2, map_opd f o)
    | JUMP o ->
      JUMP (map_opd f o)
    | BRANCH (c, o) ->
      BRANCH (map_cond f c, map_opd f o)
    | SET (c, r) ->
      SET (map_cond f c, f r)
    | LOAD (r, m) ->
      LOAD (f r, map_mem f m)
    | STORE (m, o) ->
      STORE (map_mem f m, map_opd f o)
    | MUL (r1, r2, o) ->
      MUL (f r1, f r2, map_opd f o)
    | CALL o ->
      CALL (map_opd f o)
    | RET ->
      RET
    | PHI (r, l) ->
      PHI (f r, List.map (fun {pred;r} -> { pred; r = f r }) l)
    | MACH inst ->
      MACH (I.map f inst)

  type block = {
    insts : inst list;
    succ : int list;
    name : string;
  }

  type abs_proc = {
    name : string;
    blocks : block array;
    n_reg : int;
  }

  type abs_prog = {
    procs : abs_proc array;
  }

end

module AbsMachInst = struct

  type t =
    | PUT_ARG of int * opd
    | GET_ARG of int * reg
    | PUT_RETVAL of int * opd
    | GET_RETVAL of int * reg
    | GET_FP of reg

  let map f = function
    | PUT_ARG (i, o) -> PUT_ARG (i, map_opd f o)
    | GET_ARG (i, r) -> GET_ARG (i, f r)
    | PUT_RETVAL (i, o) -> PUT_RETVAL (i, map_opd f o)
    | GET_RETVAL (i, r) -> GET_RETVAL (i, f r)
    | GET_FP r -> GET_FP (f r)

end

module Abstract = OfInstType (AbsMachInst)
