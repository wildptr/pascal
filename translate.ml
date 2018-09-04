open Batteries
open Canon
open Ir
open Abstract

type tblock = {
  mutable insts : inst list;
  name : string;
  id : int;
  mutable succ : int list;
  regtab : (reg * bool) option array
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
  | Direct of reg * int
  | Indirect of mem_loc * int

type var_info = {
  visible : bool;
  loc : mem_loc option
}

type env = {
  mutable next_reg : int;
  mutable current_block : tblock;
  mutable blocks : tblock list;
  mutable n_block : int;
  var_info : var_info array;
  config : config;
  nvar : int
}

let emit_block b inst =
  b.insts <- inst :: b.insts

let emit env inst =
  emit_block env.current_block inst

let fresh_reg env =
  let i = env.next_reg in
  env.next_reg <- i+1;
  i

let get_reg b v =
  b.regtab.(v)

let update_reg b v r m =
  b.regtab.(v) <- Some (r, m)

let kill_reg b v =
  b.regtab.(v) <- None

let rec convert_mem_loc env b = function
  | Direct (r, off) -> (Some r, imm_i off)
  | Indirect (loc, off) ->
    let m = convert_mem_loc env b loc in
    let r = fresh_reg env in
    LOAD (r, m) |> emit_block b;
    (Some r, imm_i off)

let get_loc env v =
  env.var_info.(v).loc

let get_mem env b v =
  match get_loc env v with
  | None -> failwith "variable not in memory"
  | Some loc ->
    convert_mem_loc env b loc

let is_visible env v =
  env.var_info.(v).visible

let get_addr env b v =
  let (base, off) = get_mem env b v in
  match base with
  | Some base ->
    let r = fresh_reg env in
    (ALU2 (ADD, r, base, Imm off) |> emit_block b; Reg r)
  | None -> Imm off

let to_reg env = function
  | Reg r -> r
  | o ->
    let r = fresh_reg env in
    emit env (MOV (r, o));
    r

let load_var env b v =
  let m = get_mem env b v in
  let r = fresh_reg env in
  LOAD (r, m) |> emit env;
  update_reg b v r true;
  r

let get_var env v =
  match get_reg env.current_block v with
  | Some (r, _) -> Reg r
  | None -> Reg (load_var env env.current_block v)

let fresh_block_id env =
  let i = env.n_block in
  env.n_block <- i+1;
  i

let merge_regtab env b1 b2 =
  let regtab = Array.make env.nvar None in
  let insts = ref [] in
  let make_phi v r1 r2 m =
    let r = fresh_reg env in
    regtab.(v) <- Some (r, m);
    let rhs1 = { pred = b1.id; r = r1 } in
    let rhs2 = { pred = b2.id; r = r2 } in
    insts := PHI (r, [rhs1;rhs2]) :: !insts
  in
  for v=0 to env.nvar - 1 do
    match b1.regtab.(v), b2.regtab.(v) with
    | Some (r1, m1), Some (r2, m2) ->
      if r1 = r2 then
        regtab.(v) <- Some (r1, m1&&m2)
      else
        make_phi v r1 r2 (m1&&m2)
    | Some (r1, m1), None ->
      make_phi v r1 (load_var env b2 v) m1
    | None, Some (r2, m2) ->
      make_phi v (load_var env b1 v) r2 m2
    | None, None -> ()
  done;
  regtab, !insts

let block_name_of_id i =
  Printf.sprintf ".L%d" i

let fresh_block env ?id preds =
  let id =
    match id with
    | Some id -> id
    | None -> fresh_block_id env
  in
  let name = block_name_of_id id in
  let regtab, insts =
    match preds with
    | [b] -> Array.copy b.regtab, []
    | [b1;b2] -> merge_regtab env b1 b2
    | _ -> failwith ""
  in
  let (blk:tblock) = { insts; name; id; succ = []; regtab } in
  preds |> List.iter (fun b -> add_succ b blk);
  env.blocks <- blk :: env.blocks;
  blk

let rec translate_expr env = function
  | C_IntExpr i -> imm_opd_i i
  | C_BoolExpr b -> imm_opd_i (if b then 1 else 0)
  | C_VarExpr v -> get_var env v.lid
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
  if v.isref then
    let m = get_mem env env.current_block v.lid in
    STORE (m, Reg r) |> emit env;
    update_reg env.current_block v.lid r true;
  else
    update_reg env.current_block v.lid r false

let write_back_reg env v r =
  match get_loc env v with
  | None -> ()
  | Some _ ->
    let m = get_mem env env.current_block v in
    STORE (m, Reg r) |> emit env

let write_back env v =
  match get_reg env.current_block v with
  | Some (r, m) -> if not m then write_back_reg env v r
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
    let rhs_tab = Array.init env.nvar (fun _ -> { pred = tail.id; r = 0 }) in
    let patch_rhs v r =
      rhs_tab.(v).r <- r
    in
    for v=0 to env.nvar - 1 do
      match get_reg env.current_block v with
      | Some (r, m) ->
        let rhs_pred = { pred = pred.id; r } in
        let rhs_tail = rhs_tab.(v) in
        let r' = fresh_reg env in
        PHI (r', [rhs_pred; rhs_tail]) |> emit env;
        update_reg env.current_block v r' m
      | None -> ()
    done;
    (* actually translate loop body *)
    body |> List.iter (translate_stmt env);
    let succ_id = fresh_block_id env in
    let succ_name = block_name_of_id succ_id in
    translate_cond env false (imm_opd_s succ_name) cond;
    (* maintain loop invariants *)
    env.current_block <- tail;
    for v=0 to env.nvar - 1 do
      match pred.regtab.(v), get_reg env.current_block v with
      | Some _, Some (r, _) ->
        patch_rhs v r
      | Some _, None ->
        let r = load_var env env.current_block v in
        patch_rhs v r
      | None, Some (r, _) ->
        write_back_reg env v r
      | None, None ->
        ()
    done;
    JUMP (imm_opd_s blk.name) |> emit env;
    env.current_block <- fresh_block env ~id:succ_id [blk]
  | C_CallStmt (vars, proc, args) ->
    args |> Array.iteri begin fun i arg ->
      let o =
        if proc.params.(i).byref then
          match arg with
          | C_VarExpr v -> get_addr env env.current_block v.lid
          | _ -> failwith "var argument not a variable"
        else translate_expr env arg
      in
      emit env (MACH (PUT_ARG (i, o)))
    end;
    for v=0 to env.nvar-1 do
      if is_visible env v then begin
        write_back env v;
        kill_reg env.current_block v
      end
    done;
    emit env (CALL (imm_opd_s proc.name));
    vars |> Array.iteri begin fun i v ->
      let r = fresh_reg env in
      emit env (MACH (GET_RETVAL (i, r)));
      translate_assign env v (Reg r)
    end

let translate config (info : info) (prog : program) =
  (* TODO: inner-most procedures don't need parent FP *)
  let stack_top = Array.make (Array.length prog.procs) (-4) in
  let loctab = Array.make (Array.length prog.vars) None in
  let var_depth =
    prog.vars |> Array.map (fun v -> prog.procs.(v.proc_id).head.depth)
  in
  prog.vars |> Array.iter begin fun v ->
    let vd = var_depth.(v.gid) in
    let alloc v =
      if info.vis_tab.(v.gid) then begin
        let off = stack_top.(v.proc_id) - 4 in (*TODO*)
        stack_top.(v.proc_id) <- off;
        loctab.(v.gid) <- Some (vd, off)
      end
    in
    match v.param_id with
    | Some i ->
      begin match config.param_loc i with
      | RO_Reg _ -> alloc v
      | RO_Off off -> loctab.(v.gid) <- Some (vd, off)
      end
    | None -> alloc v
  end;
  let procs =
    prog.procs |> Array.map begin fun (proc:proc) ->
      let head = proc.head in
      let nvar = Array.length proc.vars in
      let regtab = Array.make nvar None in
      (* pre-load arguments *)
      let np = Array.length head.params in
      let np' =
        if head.depth = 0 then np+1 else np
      in
      let next_reg = ref config.n_reg in
      let frame_link_loc = ref (RO_Reg 0) in
      let insts = ref [] in
      let local_start = proc.var_start.(head.depth) in
      for i=0 to np'-1 do
        match config.param_loc i with
        | RO_Reg r ->
          let r' = !next_reg in
          incr next_reg;
          insts := MOV (r', Reg r) :: !insts;
          if i < np then
            regtab.(local_start + i) <- Some (r', false)
          else
            frame_link_loc := RO_Reg r'
        | ro ->
          if i = np then
            frame_link_loc := ro
      done;
      let blk =
        { insts = !insts; name = ".L0"; id = 0; succ = []; regtab }
      in
      let var_info =
        proc.vars |> Array.map begin fun v ->
          let loc =
            loctab.(v.gid) |> Option.map begin fun (f, off) ->
              let rec wrap loc n =
                if n > 0 then wrap (Indirect (loc, 0)) (n-1) else loc
              in
              let l =
                if f < head.depth then
                  let init =
                    match !frame_link_loc with
                    | RO_Reg r -> Direct (r, 0)
                    | RO_Off p_off -> Indirect (Direct (config.fp, p_off), 0)
                  in
                  wrap init (head.depth - f - 1)
                else Direct (config.fp, off)
              in
              if v.isref then Indirect (l, 0) else l
            end
          in
          { loc; visible = info.vis_tab.(v.gid) }
        end
      in
      let env = {
        next_reg = !next_reg;
        current_block = blk;
        blocks = [blk];
        n_block = 1;
        var_info;
        config;
        nvar
      } in
      proc.body |> List.iter (translate_stmt env);
      (* write back visible outer variables and ref parameters *)
      for i=0 to local_start-1 do
        write_back env i
      done;
      for i = local_start to local_start + np - 1 do
        if proc.vars.(i).isref then write_back env i
      done;
      let blocks =
        env.blocks |> List.map finalize_block |> List.rev |> Array.of_list
      in
      let n_reg = env.next_reg in
      { blocks; n_reg }
    end
  in
  { procs }
