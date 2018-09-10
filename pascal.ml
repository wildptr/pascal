open Batteries
open Canon

let () =
  Printexc.record_backtrace true;
  let lexbuf = Lexing.from_channel stdin in
  let ast =
    try Parser.program Lexer.read lexbuf with
    | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "%d:%d: syntax error\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
  in
  let prog = Canonicalize.canon_program ast in
(*   prog.procs |> Array.iter (Canon.pp_proc Format.std_formatter); *)
  let alias_tab = Alias.build_alias_tab prog in
  let vis_tab = Visibility.build_table prog in
  let module X86_IR = Ir.MakeIR(Ir.MakeInst(X86)) in
  let module RegAllocX86 = Regalloc.Make(X86) in
  let module SSA_X86 = Ssa.Make(X86) in
  let dsa_prog = Dsa.dsa alias_tab prog in
(*   dsa_prog.procs |> Array.iter (Canon.pp_proc Format.std_formatter); *)
  dsa_prog.procs |> Array.iter begin fun proc ->
    proc
    |> Passify.passify_proc
    |> Verify.verify_proc
    |> begin function
      | Verify.Invalid -> "invalid"
      | Verify.Valid -> "valid"
      | Verify.Unknown -> "unknown"
    end
    |> Printf.eprintf "%s: %s\n" proc.head.name
  end;
  let t_info = Translate.{ vis_tab; alias_tab } in
  let abs_prog = Translate.translate X86.t_config t_info prog in
  let mach_procs =
    abs_prog |> Array.map begin fun proc ->
      proc
      |> (fun p -> Format.eprintf "%a@." Ir.pp_abs_proc p; p)
      |> LowerX86.lower_proc
      |> (fun p -> Format.eprintf "%a@." X86_IR.pp_proc p; p)
      |> RegAllocX86.allocate_registers
      |> (fun p -> SSA_X86.remove_phi p; p)
    end
  in
  X86_asm.emit_program Format.std_formatter mach_procs
