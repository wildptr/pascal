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
  prog.procs |> Array.iter (Canon.pp_proc Format.std_formatter);
  let alias_tab = Alias.build_alias_tab prog in
  let vis_tab = Visibility.build_table prog in
  let module LowerX86 = Lower.Machine(X86) in
  let module RegAllocX86 = Regalloc.Machine(X86) in
  let dsa_prog = Dsa.dsa alias_tab prog in
  dsa_prog.procs |> Array.iter (Canon.pp_proc Format.std_formatter);
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
  abs_prog.procs |> Array.iter begin fun proc ->
    proc
    |> LowerX86.lower_proc
    |> RegAllocX86.allocate_registers
    |> X86.emit_asm Format.std_formatter
  end
