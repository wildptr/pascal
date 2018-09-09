open Batteries
open Ir

module Make (M : MachineType) = struct

  module MIR = MakeIR(MakeInst(M))

  let lower_proc (proc : abs_proc) : MIR.proc =
    let blocks =
      proc.blocks |> Array.map begin fun (b : abs_block) ->
        let insts = M.lower_insts b.insts in
        MIR.{ insts; succ = b.succ; name = b.name }
      end
    in
    MIR.{ name = proc.name; blocks; n_reg = proc.n_reg }

end
