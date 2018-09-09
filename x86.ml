open Batteries
open Ir
open Format

module S = Set.Int

type opd =
  | Reg of reg
  | Imm of int
  | Label of string

type mem = {
  base : reg option;
  index : (reg * int) option;
  disp : int;
  size : int
}

let n_reg = 8
let n_reg_avail = 6
let reg_mask = lnot 0b11001111
let reg_name = [|"eax";"ecx";"edx";"ebx";"esp";"ebp";"esi";"edi"|]

type unary =
  | NOT
  | NEG

type binary =
  | ADD
  | SUB
  | AND
  | OR
  | XOR
  | MUL

type cond1 =  Z | NZ
type cond2 = EQ | NE | LT | GE | GT | LE

type cond =
  | Cond1 of cond1 * reg
  | Cond2 of cond2 * reg * opd

type inst =
  | MOV of reg * opd
  | UNARY of unary * reg * reg
  | BINARY of binary * reg * reg * opd
  | LOAD of reg * mem
  | STORE of mem * opd
  | LEA of reg * mem
  | JMP of opd
  | CJMP of cond * opd
  | SET of cond * reg
  | CALL of opd
  | RET
  | PUSH of opd
  | PHI of reg * phi_rhs list

let opd_uses = function
  | Reg r -> S.singleton r
  | _ -> S.empty

let cond_uses = function
  | Cond1 (_, r) -> S.singleton r
  | Cond2 (_, r, o) -> S.add r (opd_uses o)

let mem_uses { base; index; disp } =
  let s =
    match base with
    | Some r -> S.singleton r
    | None -> S.empty
  in
  match index with
  | Some (r, i) -> S.add r s
  | None -> s

let defs = function
  | MOV (r, _) -> S.singleton r
  | UNARY (_, r, _) -> S.singleton r
  | BINARY (_, r, _, _) -> S.singleton r
  | SET (_, r) -> S.singleton r
  | LOAD (r, _) -> S.singleton r
  | CALL _ -> [0;1;2] |> S.of_list
  | PHI (r, _) -> S.singleton r
  | LEA (r, _) -> S.singleton r
  | _ -> S.empty

let uses = function
  | MOV (_, o) -> opd_uses o
  | UNARY (_, _, r) -> S.singleton r
  | BINARY (_, _, r, o) -> S.add r (opd_uses o)
  | JMP o -> opd_uses o
  | CJMP (_, o) -> opd_uses o
  | SET (c, _) -> cond_uses c
  | LOAD (_, m) -> mem_uses m
  | STORE (m, o) -> S.union (mem_uses m) (opd_uses o)
  | CALL o -> opd_uses o
  | RET -> S.empty
  | PHI (_, l) -> List.fold_right (fun rhs s -> S.add rhs.r s) l S.empty
  | PUSH o -> opd_uses o
  | LEA (_, m) -> mem_uses m

let t_config =
  Translate.
    { fp = 5;
      n_reg;
      param_loc = (fun i -> RO_Off (8+4*i));
      retval_loc = (fun i -> i) }

let pp_reg f r =
  if r < 8 then
    pp_print_string f reg_name.(r)
  else
    fprintf f "r%d" r

let pp_opd f = function
  | Reg r -> pp_reg f r
  | Imm i -> pp_print_int f i
  | Label s -> pp_print_string f s

let pp_mem f { base; index; disp; size } =
  let s =
    match size with
    | 1 -> "byte"
    | 2 -> "word"
    | 4 -> "dword"
    | _ -> failwith "pp_mem"
  in
  fprintf f "%s [" s;
  let sep = ref false in
  begin match base with
    | Some r ->
      pp_reg f r;
      sep := true
    | None -> ()
  end;
  begin match index with
    | Some (r, i) ->
      if !sep then pp_print_char f '+';
      fprintf f "%a*%d" pp_reg r (1 lsl i);
      sep := true
    | None -> ()
  end;
  if !sep then
    (if disp <> 0 then fprintf f "%+d" disp)
  else
    pp_print_int f disp;
  pp_print_char f ']'

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
  | UNARY (op, r1, r2) ->
    let m =
      match op with
      | NOT -> "not"
      | NEG -> "neg"
    in
    if r1 <> r2 then
      fprintf f "\tmov\t%a, %a\n" pp_reg r1 pp_reg r2;
    fprintf f "\t%s\t%a\n" m pp_reg r1
  | BINARY (op, r1, r2, o) ->
    let m =
      match op with
      | ADD -> "add"
      | SUB -> "sub"
      | MUL -> "imul"
      | AND -> "and"
      | OR -> "or"
      | XOR -> "xor"
    in
    if r1 <> r2 then
      fprintf f "\tmov\t%a, %a\n" pp_reg r1 pp_reg r2;
    fprintf f "\t%s\t%a, %a\n" m pp_reg r1 pp_opd o
  | JMP o ->
    fprintf f "\tjmp\t%a\n" pp_opd o
  | CJMP (c, o) ->
    emit_cond f "j" c pp_opd o
  | SET (c, r) ->
    emit_cond f "set" c pp_reg r
  | LOAD (r, m) ->
    fprintf f "\tmov\t%a, %a\n" pp_reg r pp_mem m
  | STORE (m, o) ->
    fprintf f "\tmov\t%a, %a\n" pp_mem m pp_opd o
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
  | PUSH o ->
    fprintf f "\tpush\t%a\n" pp_opd o
  | LEA (r, m) ->
    fprintf f "\tlea\t%a, %a\n" pp_reg r pp_mem m

let map_opd f = function
  | Reg r -> Reg (f r)
  | o -> o

let map_mem f { base; index; disp; size } =
  { base = Option.map f base;
    index = (match index with None -> None | Some (r, i) -> Some (f r, i));
    disp; size }

let map_cond f = function
  | Cond1 (op, r) ->
    Cond1 (op, f r)
  | Cond2 (op, r, o) ->
    Cond2 (op, f r, map_opd f o)

let map_inst f = function
  | MOV (r, o) ->
    MOV (f r, map_opd f o)
  | UNARY (op, r1, r2) ->
    UNARY (op, f r1, f r2)
  | BINARY (op, r1, r2, o) ->
    BINARY (op, f r1, f r2, map_opd f o)
  | JMP o ->
    JMP (map_opd f o)
  | CJMP (c, o) ->
    CJMP (map_cond f c, map_opd f o)
  | SET (c, r) ->
    SET (map_cond f c, f r)
  | LOAD (r, m) ->
    LOAD (f r, map_mem f m)
  | STORE (m, o) ->
    STORE (map_mem f m, map_opd f o)
  | CALL o ->
    CALL (map_opd f o)
  | RET ->
    RET
  | PHI (r, l) ->
    PHI (f r, List.map (fun {pred;r} -> { pred; r = f r }) l)
  | PUSH o ->
    PUSH (map_opd f o)
  | LEA (r, m) ->
    LEA (f r, map_mem f m)

let move_related_pair = function
  | MOV (r1, Reg r2) -> Some (r1, r2)
  | UNARY (_, r1, r2) -> Some (r1, r2)
  | BINARY (_, r1, r2, _) -> Some (r1, r2)
  | _ -> None
