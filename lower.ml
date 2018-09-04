open Batteries
open Ir
open Inst

module Machine (M : MachineType) = struct

  module MIR = OfInstType(M.SpecInst)
  module LiftM = Lift(M)

  open Abstract

  let lower_proc proc : MIR.abs_proc =
    let blocks =
      proc.blocks |> Array.map begin fun blk ->
        let insts = blk.insts |> List.map LiftM.lift_inst in
        MIR.{ insts; succ = blk.succ; name = blk.name }
      end
    in
    MIR.{ name = proc.name; blocks; n_reg = proc.n_reg }

end
