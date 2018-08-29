open Batteries

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
  let procs = Canonicalize.canon_program ast in
  (*let procs_dsa = procs |> List.map Dsa.dsa_proc in
  procs_dsa |> List.iter (Format.printf "%a@." Canon.pp_proc);
  procs_dsa |> List.iter begin fun proc ->
    let passive_proc = Passify.passify_proc proc in
    let status =
      match Verify.verify_proc passive_proc with
      | Verify.Invalid -> "invalid"
      | Verify.Valid -> "valid"
      | Verify.Unknown -> "unknown"
    in
    print_endline status
  end*)
  let module LowerX86 = Lower.Machine(X86) in
  let module RegAllocX86 = Regalloc.Machine(X86) in
  procs |> List.iter begin fun proc ->
    proc
    |> Translate.translate_proc
    |> LowerX86.lower_proc
    |> RegAllocX86.allocate_registers
    |> X86.emit_asm Format.std_formatter
  end
