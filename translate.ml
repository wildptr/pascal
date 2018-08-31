open Batteries
open Canon
open Ir
open Abstract

type tblock = {
  mutable insts : inst list;
  name : string;
  id : int;
  mutable succ : int list;
  cache : reg option array
}

let add_succ b1 b2 =
  b1.succ <- b2.id :: b1.succ

let finalize_block blk : block =
  let insts = List.rev blk.insts in
  { insts; succ = blk.succ; name = blk.name }

type reg_or_offset =
  | RO_Reg of int
  | RO_Off of int

type config = {
  fp : reg;
  n_reg : int;
  param_loc : int -> reg_or_offset
}

type info = {
  vis_tab : bool array
}

type mem_loc =
  | L_None
  | L_Mem of int * int * bool

type env = {
  mutable next_reg : int;
  mutable current_block : tblock;
  mutable blocks : tblock list;
  mutable n_block : int;
  vis_tab : bool array; (* gid-indexed *)
  proc_depth : int;
  loc_tab : mem_loc array; (* gid-indexed *)
  config : config;
  vars : var array
}

let emit_block b inst =
  b.insts <- inst :: b.insts

let emit env inst =
  emit_block env.current_block inst

let fresh_reg env =
  let i = env.next_reg in
  env.next_reg <- i+1;
  i

let get_fp env f =
  env.config.fp

let get_mem env b (f, off, isref) =
  let fp = get_fp env f in
  let m = (Some fp, imm_i off) in
  if isref then begin
    let r = fresh_reg env in
    LOAD (r, m) |> emit_block b;
    (Some r, imm_i 0)
  end else m

let get_loc env v =
  env.loc_tab.(v.gid)

let get_addr env b v =
  let f, off, isref =
    match get_loc env v with
    | L_None -> failwith "variable not in memory"
    | L_Mem (f, off, isref) -> f, off, isref
  in
  let fp = get_fp env f in
  let r = fresh_reg env in
  if isref then
    (LOAD (r, (Some fp, imm_i off)) |> emit_block b; r)
  else
    (ALU2 (ADD, r, fp, imm_opd_i off) |> emit_block b; r)

let to_reg env = function
  | Reg r -> r
  | o ->
    let r = fresh_reg env in
    emit env (MOV (r, o));
    r

let get_cache b v =
  b.cache.(v.lid)

let update_cache b v r =
  b.cache.(v.lid) <- Some r

let kill b v =
  b.cache.(v.lid) <- None

let load_var env b v =
  match get_loc env v with
  | L_None -> failwith "variable not in memory"
  | L_Mem (f, off, isref) ->
    let m = get_mem env b (f, off, isref) in
    let r = fresh_reg env in
    LOAD (r, m) |> emit env;
    update_cache b v r;
    r

let fresh_block_id env =
  let i = env.n_block in
  env.n_block <- i+1;
  i

let merge_cache env b1 b2 =
  let cache = Array.make (Array.length env.vars) None in
  let make_phi v r1 r2 =
    let r = fresh_reg env in
    cache.(v.lid) <- Some r;
    let rhs1 = { pred = b1.id; r = r1 } in
    let rhs2 = { pred = b2.id; r = r2 } in
    PHI (r, [rhs1;rhs2])
  in
  let insts =
    Array.fold_right begin fun v insts ->
      match b1.cache.(v.lid), b2.cache.(v.lid) with
      | Some r1, Some r2 ->
        make_phi v r1 r2 :: insts
      | Some r1, None ->
        make_phi v r1 (load_var env b2 v) :: insts
      | None, Some r2 ->
        make_phi v (load_var env b1 v) r2 :: insts
      | None, None -> insts
    end env.vars []
  in
  cache, insts

let block_name_of_id i =
  Printf.sprintf ".L%d" i

let fresh_block env ?id preds =
  let id =
    match id with
    | Some id -> id
    | None -> fresh_block_id env
  in
  let name = block_name_of_id id in
  let cache, insts =
    match preds with
    | [b] -> b.cache, []
    | [b1;b2] -> merge_cache env b1 b2
    | _ -> failwith ""
  in
  let (blk:tblock) = { insts; name; id; succ = []; cache } in
  preds |> List.iter (fun b -> add_succ b blk);
  env.blocks <- blk :: env.blocks;
  blk

let translate_var env v =
  match get_cache env.current_block v with
  | Some r -> Reg r
  | None -> Reg (load_var env env.current_block v)

let rec translate_expr env = function
  | C_IntExpr i -> imm_opd_i i
  | C_BoolExpr b -> imm_opd_i (if b then 1 else 0)
  | C_VarExpr v -> translate_var env v
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

let negate_cond2 = function
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
        let cond = Cond2 ((if neg then negate_cond2 c else c), s1, s2) in
        BRANCH (cond, target) |> emit env
      | None -> translate_cond_generic env neg target e
    end
  | e -> translate_cond_generic env neg target e

let translate_assign env (v:var) o =
  let r = fresh_reg env in
  MOV (r, o) |> emit env;
  (* TODO kill aliases *)
  update_cache env.current_block v r

let write_back_reg env v r =
  match get_loc env v with
  | L_None -> ()
  | L_Mem (f, off, isref) ->
    let m = get_mem env env.current_block (f, off, isref) in
    STORE (m, Reg r) |> emit env

let write_back env v =
  match get_cache env.current_block v with
  | Some r -> write_back_reg env v r
  | None -> ()

let rec translate_stmt env = function
  | C_AssignStmt (lhs, rhs) ->
    let s = translate_expr env rhs in
    translate_assign env lhs s
  | C_AssertStmt _ | C_AssumeStmt _ -> ()
  | C_IfStmt (cond, bodyT, bodyF) ->
    let blkT = fresh_block env [env.current_block] in
    let blkF = fresh_block env [env.current_block] in

    translate_cond env true (imm_opd_s blkF.name) cond;

    env.current_block <- blkT;
    bodyT |> List.iter (translate_stmt env);

    env.current_block <- blkF;
    bodyF |> List.iter (translate_stmt env);

    let join = fresh_block env [blkT; blkF] in
    JUMP (imm_opd_s join.name) |> emit_block blkT;
    env.current_block <- join

  | C_RepeatStmt (_, body, cond) ->
    let pred = env.current_block in
    let blk = fresh_block env [pred] in
    let tail = fresh_block env [blk] in
    add_succ tail blk;
    env.current_block <- blk;
    (* insert phi instructions *)
    let rhs_tab = Array.map (fun _ -> { pred = tail.id; r = 0 }) env.vars in
    let patch_rhs v r =
      rhs_tab.(v.lid).r <- r
    in
    env.vars |> Array.iter begin fun v ->
      match get_cache env.current_block v with
      | Some r ->
        let rhs_pred = { pred = pred.id; r } in
        let rhs_tail = rhs_tab.(v.lid) in
        let r' = fresh_reg env in
        PHI (r', [rhs_pred; rhs_tail]) |> emit env;
        update_cache env.current_block v r'
      | None -> ()
    end;
    (* actually translate loop body *)
    body |> List.iter (translate_stmt env);
    let succ_id = fresh_block_id env in
    let succ_name = block_name_of_id succ_id in
    translate_cond env false (imm_opd_s succ_name) cond;
    (* maintain loop invariants *)
    env.current_block <- tail;
    env.vars |> Array.iter begin fun v ->
      match pred.cache.(v.lid), get_cache env.current_block v with
      | Some _, Some r ->
        patch_rhs v r
      | Some _, None ->
        let r = load_var env env.current_block v in
        patch_rhs v r
      | None, Some r ->
        write_back_reg env v r
      | None, None ->
        ()
    end;
    JUMP (imm_opd_s blk.name) |> emit env;
    env.current_block <- fresh_block env ~id:succ_id [blk]
  | C_CallStmt (vars, proc, args) ->
    args |> Array.iteri begin fun i arg ->
      let o =
        if proc.params.(i).byref then
          match arg with
          | C_VarExpr v -> Reg (get_addr env env.current_block v)
          | _ -> failwith "var argument not a variable"
        else translate_expr env arg
      in
      emit env (MACH (PUT_ARG (i, o)))
    end;
    env.vars |> Array.iter begin fun v ->
      if env.vis_tab.(v.gid) then begin
        write_back env v;
        kill env.current_block v
      end
    end;
    emit env (CALL (imm_opd_s proc.name));
    vars |> Array.iteri begin fun i v ->
      let r = fresh_reg env in
      emit env (MACH (GET_RETVAL (i, r)));
      translate_assign env v (Reg r)
    end

let translate config (info : info) (prog : program) =
  let stack_top = Array.make (Array.length prog.procs) 0 in
  let loc_tab = Array.make (Array.length prog.vars) L_None in
  let var_depth =
    prog.vars |> Array.map (fun v -> prog.procs.(v.proc_id).head.depth)
  in
  prog.vars |> Array.iter begin fun v ->
    let vd = var_depth.(v.gid) in
    let alloc v =
      if info.vis_tab.(v.gid) then begin
        let off = stack_top.(v.proc_id) - 4 in (*TODO*)
        stack_top.(v.proc_id) <- off;
        loc_tab.(v.gid) <- L_Mem (vd, off, v.isref)
      end
    in
    match v.param_id with
    | Some i ->
      begin match config.param_loc i with
      | RO_Reg _ -> alloc v
      | RO_Off off -> loc_tab.(v.gid) <- L_Mem (vd, off, v.isref)
      end
    | None -> alloc v
  end;
  let procs =
    prog.procs |> Array.map begin fun (proc:proc) ->
      let blk =
        let cache = Array.make (Array.length proc.vars) None in
        { insts = []; name = ".L0"; id = 0; succ = []; cache }
      in
      let proc_depth = proc.head.depth in
      let env = {
        next_reg = config.n_reg;
        current_block = blk;
        blocks = [blk];
        n_block = 1;
        vis_tab = info.vis_tab;
        proc_depth;
        loc_tab;
        config;
        vars = proc.vars
      } in
      proc.body |> List.iter (translate_stmt env);
      proc.vars |> Array.iter begin fun v ->
        (* write back visible outer variables and ref parameters *)
        if var_depth.(v.gid) < proc_depth || v.isref then
          write_back env v
      end;
      let blocks =
        env.blocks |> List.map finalize_block |> List.rev |> Array.of_list
      in
      let n_reg = env.next_reg in
      { blocks; n_reg }
    end
  in
  { procs }
