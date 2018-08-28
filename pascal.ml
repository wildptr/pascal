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
  let procs_dsa = procs |> List.map Dsa.dsa_proc in
  procs_dsa |> List.iter (Format.printf "%a@." Canon.pp_proc);
  procs_dsa |> List.iter begin fun proc ->
    let passive_proc = Passify.passify_proc proc in
    let status =
      match Verify.verify_proc passive_proc with
      | Verify.Unsat -> "unsat"
      | Verify.Unknown -> "unknown"
      | Verify.Sat -> "sat"
    in
    print_endline status
  end
