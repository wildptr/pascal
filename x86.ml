open Batteries
open Ir
open Inst

module S = Set.Int

module SpecInst = struct

  type t =
    | PUSH of opd

  let map f = function
    | PUSH o -> PUSH (map_opd f o)

end

module MIR = OfInstType(SpecInst)
open MIR

let defs = function
  | SpecInst.PUSH _ -> S.empty

let call_defs = [0;1;2] |> S.of_list

let uses = function
  | SpecInst.PUSH o -> opd_uses o

let translate_inst = function
  | AbsMachInst.PUT_ARG (_, o) -> MACH (PUSH o)
  | AbsMachInst.GET_ARG (i, r) -> LOAD (r, (Some 5, imm_i (8+4*i)))
  | AbsMachInst.PUT_RETVAL (i, o) -> MOV (i, o)
  | AbsMachInst.GET_RETVAL (i, r) -> MOV (r, Reg i)
  | AbsMachInst.GET_FP r -> MOV (r, Reg 5)

let n_reg = 8
let n_reg_avail = 6
let reg_mask = lnot 0b11001111
let reg_name = [|"eax";"ecx";"edx";"ebx";"esp";"ebp";"esi";"edi"|]

let t_config = Translate.
  { fp = 5;
    n_reg;
    param_loc = fun i -> RO_Off (8+4*i) }

open Format

let pp_reg f r =
  if r >= 0 && r < 8 then
    pp_print_string f reg_name.(r)
  else fprintf f "r%d" r

let pp_imm f { s; i } =
  if s = "" then
    pp_print_int f i
  else
    (if i = 0 then
       pp_print_string f s
     else
       fprintf f "%s%+d" s i)

let pp_opd f = function
  | Reg r -> pp_reg f r
  | Imm i -> pp_imm f i

let pp_mem f (base, offset) =
  match base with
  | Some base ->
    fprintf f "[%a" pp_reg base;
    if offset.s = "" then
      if offset.i = 0 then
        pp_print_char f ']'
      else
        fprintf f "%+d]" offset.i
    else
      fprintf f "+%a]" pp_imm offset
  | None ->
    fprintf f "[%a]" pp_imm offset

let emit_cond f m c pp_t t =
  let s =
    match c with
    | Cond1 (op, r) ->
      fprintf f "\ttest\t%a, %a\n" pp_reg r pp_reg r;
      begin match op with
        | Z -> "e"
        | NZ -> "ne"
      end
    | Cond2 (op, r, o) ->
      fprintf f "\tcmp\t%a, %a\n" pp_reg r pp_opd o;
      begin match op with
        | EQ -> "e"
        | NE -> "ne"
        | LT -> "l"
        | GE -> "ge"
        | GT -> "g"
        | LE -> "le"
      end
  in
  fprintf f "\t%s%s\t%a\n" m s pp_t t

let emit_inst f = function
  | MOV (r, o) ->
    if Reg r <> o then
      fprintf f "\tmov\t%a, %a\n" pp_reg r pp_opd o
  | ALU1 (op, r1, r2) ->
    let m =
      match op with
      | NOT -> "not"
      | NEG -> "neg"
    in
    if r1 <> r2 then
      fprintf f "\tmov\t%a, %a\n" pp_reg r1 pp_reg r2;
    fprintf f "\t%s\t%a\n" m pp_reg r1
  | ALU2 (op, r1, r2, o) ->
    let m =
      match op with
      | ADD -> "add"
      | SUB -> "sub"
      | AND -> "and"
      | OR -> "or"
      | XOR -> "xor"
    in
    if r1 <> r2 then
      fprintf f "\tmov\t%a, %a\n" pp_reg r1 pp_reg r2;
    fprintf f "\t%s\t%a, %a\n" m pp_reg r1 pp_opd o
  | JUMP o ->
    fprintf f "\tjmp\t%a\n" pp_opd o
  | BRANCH (c, o) ->
    emit_cond f "j" c pp_opd o
  | SET (c, r) ->
    emit_cond f "set" c pp_reg r
  | LOAD (r, m) ->
    fprintf f "\tmov\t%a, %a\n" pp_reg r pp_mem m
  | STORE (m, o) ->
    fprintf f "\tmov\t%a, %a\n" pp_mem m pp_opd o
  | MUL (r1, r2, o) ->
    if r1 <> r2 then
      fprintf f "\tmov\t%a, %a\n" pp_reg r1 pp_reg r2;
    fprintf f "\timul\t%a, %a\n" pp_reg r1 pp_opd o
  | CALL o ->
    fprintf f "\tcall\t%a\n" pp_opd o
  | RET ->
    fprintf f "\tret\n"
  | PHI (r, l) ->
    fprintf f "\tPHI\t%a" pp_reg r;
    let pp_rhs f { pred; r } =
      fprintf f "%d:%a" pred pp_reg r
    in
    List.iter (fprintf f ", %a" pp_rhs) l;
    pp_print_char f '\n'
  | MACH (PUSH o) ->
    fprintf f "\tpush\t%a\n" pp_opd o

let emit_block f (blk:block) =
  fprintf f "%s:\n" blk.name;
  blk.insts |> List.iter (emit_inst f)

let emit_asm f (proc : abs_proc) =
  fprintf f "%s:\n" proc.name;
  proc.blocks |> Array.iter (emit_block f)
