open Batteries
open Format

type reg = int

type abs_unary_op =
  | I_NOT
  | I_NEG
  | I_Z
  | I_NZ

type abs_binary_op =
  | I_ADD
  | I_SUB
  | I_MUL
  | I_AND
  | I_OR
  | I_XOR
  | I_EQ
  | I_NE
  | I_LT
  | I_GE
  | I_GT
  | I_LE

type abs_expr =
  | I_Reg of reg
  | I_Imm of int
  | I_Label of string
  | I_Unary of abs_unary_op * abs_expr
  | I_Binary of abs_binary_op * abs_expr * abs_expr
  | I_Load of int * abs_expr

type phi_rhs = { pred : int; mutable r : reg }

type abs_inst =
  | I_Set of reg * abs_expr
  | I_Store of int * abs_expr * abs_expr
  | I_Jump of abs_expr
  | I_Branch of abs_expr * abs_expr
  | I_Call of abs_expr * reg list
  | I_Return
  | I_Phi of reg * phi_rhs list

module type InstType = sig
  type t
  val emit : formatter -> t -> unit
end

module MakeIR (I : InstType) = struct

  type block = {
    insts : I.t list;
    succ : int list;
    name : string
  }

  type proc = {
    name : string;
    blocks : block array;
    n_reg : int
  }

  let pp_block f (b:block) =
    fprintf f "%s:\n" b.name;
    b.insts |> List.iter (I.emit f)

  let pp_proc f proc =
    fprintf f "procedure %s\n" proc.name;
    proc.blocks |> Array.iter (pp_block f)

end

let pp_reg f r =
  fprintf f "r%d" r

let rec pp_abs_expr f = function
  | I_Reg r -> pp_reg f r
  | I_Imm i -> pp_print_int f i
  | I_Label s -> pp_print_string f s
  | I_Unary (op, e) ->
    let s =
      match op with
      | I_NOT -> "NOT"
      | I_NEG -> "NEG"
      | I_Z -> "Z"
      | I_NZ -> "NZ"
    in
    fprintf f "%s(%a)" s pp_abs_expr e
  | I_Binary (op, e1, e2) ->
    let s =
      match op with
      | I_ADD -> "ADD"
      | I_SUB -> "SUB"
      | I_MUL -> "MUL"
      | I_AND -> "AND"
      | I_OR  -> "OR"
      | I_XOR -> "XOR"
      | I_EQ  -> "EQ"
      | I_NE  -> "NE"
      | I_LT  -> "LT"
      | I_GE  -> "GE"
      | I_GT  -> "GT"
      | I_LE  -> "LE"
    in
    fprintf f "%s(%a, %a)" s pp_abs_expr e1 pp_abs_expr e2
  | I_Load (n, e) ->
    fprintf f "[%a;%d]" pp_abs_expr e n

let pp_list pp f = function
  | [] -> ()
  | hd::tl ->
    pp f hd;
    List.iter (fprintf f ", %a" pp) tl

let emit_abs_inst f = function
  | I_Set (r, e) ->
    fprintf f "\t%a := %a\n" pp_reg r pp_abs_expr e
  | I_Store (n, addr, e) ->
    fprintf f "\t[%a;%d] := %a\n" pp_abs_expr addr n pp_abs_expr e
  | I_Jump e ->
    fprintf f "\tGOTO %a\n" pp_abs_expr e
  | I_Branch (cond, e) ->
    fprintf f "\tGOTO %a IF %a\n" pp_abs_expr e pp_abs_expr cond
  | I_Call (e, l) ->
    fprintf f "\tCALL %a(%a)\n" pp_abs_expr e (pp_list pp_reg) l
  | I_Return ->
    pp_print_string f "\tRETURN\n"
  | I_Phi (r, l) ->
    let pp_rhs f { pred; r } =
      fprintf f "%d:%a" pred pp_reg r
    in
    fprintf f "\t%a := PHI(%a)\n" pp_reg r (pp_list pp_rhs) l

module Abstract = MakeIR
  (struct
    type t = abs_inst
    let emit = emit_abs_inst
  end)

type abs_block = Abstract.block
type abs_proc = Abstract.proc

let pp_abs_proc = Abstract.pp_proc

module type MachineType = sig
  type inst
  val emit_inst : formatter -> inst -> unit
  val map_inst : (reg -> reg) -> inst -> inst
  val is_phi : inst -> bool
  val destruct_phi : inst -> reg * phi_rhs list
  val mk_mov : reg -> reg -> inst
  val defs : inst -> Set.Int.t
  val uses : inst -> Set.Int.t
  val move_related_pairs : inst -> (reg * reg) list
  val n_reg : int
  val n_reg_avail : int
  val reg_mask : int
end

module MakeInst (M : MachineType) = struct
  type t = M.inst
  let emit = M.emit_inst
end
