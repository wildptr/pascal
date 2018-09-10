open Batteries
open Ir
open Format

module X86_IR = MakeIR(MakeInst(X86))
open X86_IR

let emit_prologue f p =
  pp_print_string f "\tpush\tebp\n\tmov\tebp, esp\n";
  if p.frame_size > 0 then
    fprintf f "\tsub\tesp, %d\n" p.frame_size

let emit_epilogue f p =
  fprintf f "\tleave\n\tret\t%d\n" p.stack_arg_size

let emit_proc f p =
  fprintf f "%s:\n" p.name;
  emit_prologue f p;
  p.blocks |> Array.iter (pp_block f);
  emit_epilogue f p

let emit_program f procs =
  fprintf f "[bits 32]\n";
  Array.iter (emit_proc f) procs
