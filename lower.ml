open Batteries
open Ir
open Inst

module Machine (M : MachineType) = struct

  module MInst = Inst.Machine(M)
  module MIR = OfInstType(M.SpecInst)
  module LiftM = Lift(M)

  open Abstract

  let lower_inst inst =
    inst |> Abstract.map_inst (fun i -> M.k + i) |> LiftM.lift_inst

  let lower_proc proc : MIR.proc =
    let blocks =
      proc.blocks |> Array.map begin fun blk ->
        let insts = blk.insts |> List.map lower_inst in
        MIR.{ insts; succ = blk.succ; name = blk.name }
      end
    in
    let n_reg = proc.n_reg + M.k in
    MIR.{ blocks; n_reg }

end
