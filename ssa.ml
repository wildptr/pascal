open Batteries
open Ir

module Make (M : MachineType) = struct

  module MIR = MakeIR(MakeInst(M))

  let remove_phi (p : MIR.proc) =
    let n = Array.length p.blocks in
    let append_list = Array.make n [] in
    let non_phi_part = Array.make n [] in
    let final_jump = Array.make n [] in
    for i=0 to n-1 do
      let phis, nonphis, fj =
        let rec loop p = function
          | hd::tl when M.is_phi hd ->
            loop (hd::p) tl
          | l ->
            let np, fj =
              match l with
              | [] -> [], []
              | hd::tl ->
                let [@warning "-8"] last :: rest = List.rev l in
                if M.is_jump last then
                  List.rev rest, [last]
                else
                  l, []
            in
            p, np, fj
        in
        loop [] p.blocks.(i).insts
      in
      phis |> List.iter begin fun phi ->
        let lhs, rhs = M.destruct_phi phi in
        rhs |> List.iter begin fun { pred; r } ->
          append_list.(pred) <- M.mk_mov lhs r :: append_list.(pred)
        end
      end;
      non_phi_part.(i) <- nonphis;
      final_jump.(i) <- fj
    done;
    for i=0 to n-1 do
      let insts =
        List.concat [non_phi_part.(i); append_list.(i); final_jump.(i)]
      in
      p.blocks.(i) <- { p.blocks.(i) with insts }
    done

end
