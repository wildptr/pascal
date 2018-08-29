open Batteries

type reg = int

type imm = { s : string; i : int }

type opd =
  | Reg of reg
  | Imm of imm

let mk_imm_s s = Imm { s; i = 0 }
let mk_imm_i i = Imm { s = ""; i }
let mk_imm s i = Imm { s; i }

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
    | MACH inst ->
      MACH (I.map f inst)

  type block = {
    insts : inst list;
    succ : int list;
    name : string;
  }

  type proc = {
    blocks : block array;
    n_reg : int;
  }

end

module AbsMachInst = struct

  type t =
    | ARG of int * opd
    | RETVAL of int * reg

  let map f = function
    | ARG (i, o) -> ARG (i, map_opd f o)
    | RETVAL (i, r) -> RETVAL (i, f r)

end

module Abstract = OfInstType (AbsMachInst)
