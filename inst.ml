open Batteries
open Ir

module S = Set.Int

let opd_uses = function
  | Reg r -> S.singleton r
  | Imm _ -> S.empty

let cond_uses = function
  | Cond1 (_, r) -> S.singleton r
  | Cond2 (_, r, o) -> S.add r (opd_uses o)

let mem_uses (base, offset) =
  match base with
  | Some r -> S.singleton r
  | None -> S.empty

module type MachineType = sig
  module SpecInst : SpecInstType
  val translate_inst : AbsMachInst.t -> OfInstType(SpecInst).inst
  val defs : SpecInst.t -> Set.Int.t
  val uses : SpecInst.t -> Set.Int.t
  val call_defs : Set.Int.t
  val k : int
end

module Machine (M : MachineType) = struct

  module MIR = OfInstType(M.SpecInst)
  open MIR

  let defs = function
    | MOV (r, _) -> S.singleton r
    | ALU1 (_, r, _) -> S.singleton r
    | ALU2 (_, r, _, _) -> S.singleton r
    | SET (_, r) -> S.singleton r
    | LOAD (r, _) -> S.singleton r
    | MUL (r, _, _) -> S.singleton r
    | CALL _ -> M.call_defs
    | MACH inst -> M.defs inst
    | _ -> S.empty

  let uses = function
    | MOV (_, o) -> opd_uses o
    | ALU1 (_, _, r) -> S.singleton r
    | ALU2 (_, _, r, o) -> S.add r (opd_uses o)
    | JUMP o -> opd_uses o
    | BRANCH (_, o) -> opd_uses o
    | SET (c, _) -> cond_uses c
    | LOAD (_, m) -> mem_uses m
    | STORE (m, o) -> S.union (mem_uses m) (opd_uses o)
    | MUL (_, r, o) -> S.add r (opd_uses o)
    | CALL o -> opd_uses o
    | RET -> S.empty
    | MACH inst -> M.uses inst

end

open Abstract

module Lift (M : MachineType) = struct
  module MIR = OfInstType(M.SpecInst)
  let lift_inst = function
    | MACH m ->
      M.translate_inst m
    | inst -> Obj.magic inst
end
