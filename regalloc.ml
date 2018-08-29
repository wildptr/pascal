open Batteries
open Ir
open Inst

module S = Set.Int

module Machine (M : MachineType) = struct

  module MIR = OfInstType(M.SpecInst)
  module MInst = Inst.Machine(M)

  open MIR
  open MInst

  let allocate_registers proc =

    (* liveness analysis *)

    let n = Array.length proc.blocks in
    let nr = proc.n_reg in

    let def       = Array.make n S.empty in
    let use       = Array.make n S.empty in
    let live_in   = Array.make n S.empty in
    let live_out  = Array.make n S.empty in

    let pred = Array.make n [] in

    for i=0 to n-1 do
      proc.blocks.(i).insts |> List.iter begin fun inst ->
        use.(i) <- S.union use.(i) (S.diff (uses inst) def.(i));
        def.(i) <- S.union def.(i) (defs inst)
      end;
      proc.blocks.(i).succ |> List.iter (fun j -> pred.(j) <- i :: pred.(j))
    done;

    let changed = ref false in
    let rec loop () =
      for i=n-1 downto 0 do
        live_in.(i) <- S.union use.(i) (S.diff live_out.(i) def.(i));
        pred.(i) |> List.iter begin fun j ->
          let tmp = S.union live_out.(j) live_in.(i) in
          if not (S.equal tmp live_out.(j)) then
            (changed := true; live_out.(j) <- tmp)
        end;
      done;
      if !changed then (changed := false; loop ())
    in
    loop ();

    (* build interference graph *)

    let module G = Graph.Imperative.Graph.Concrete (MyInt) in
    let g = G.create ~size:nr () in
    for i=0 to nr-1 do
      G.add_vertex g i
    done;

    let connect live =
      let a = S.to_array live in
      let n = Array.length a in
      for i=0 to n-1 do
        for j=i+1 to n-1 do
          Printf.eprintf "%d -- %d\n" a.(i) a.(j);
          G.add_edge g a.(i) a.(j)
        done
      done
    in

    for i=0 to n-1 do
      let live = live_out.(i) in
      connect live;
      List.fold_right begin fun inst live ->
        let live' = S.union (S.diff live (defs inst)) (uses inst) in
        connect live';
        live'
      end proc.blocks.(i).insts live |> ignore
    done;

    (* register allocation *)

    let neighbors = Array.make nr [] in
    for i=M.k to nr-1 do
      neighbors.(i) <- G.succ g i
    done;

    let color_order = Array.make nr 0 in
    begin
      let p = ref nr in
      let removed = Array.make nr false in
      let rec loop old_p =
        for i=0 to nr-1 do
          if not removed.(i) && G.out_degree g i < M.k then begin
            decr p;
            color_order.(!p) <- i;
            G.remove_vertex g i;
            removed.(i) <- true
          end
        done;
        if !p < old_p then loop !p
      in
      loop nr;
      while !p > 0 do
        for i=0 to nr-1 do
          if not removed.(i) then (decr p; color_order.(!p) <- i)
        done
      done
    end;

    let color = Array.make nr (-1) in
    (* pre-color physical registers *)
    for i=0 to M.k-1 do color.(i) <- i done;
    for i=M.k to nr-1 do
      let f = ref (- 1 lsl M.k) in
      neighbors.(i) |> List.iter begin fun j ->
        let c = color.(j) in
        if c >= 0 then f := !f lor (1 lsl c)
      end;
      let rec loop c =
        if c = M.k then -1
        else if !f land (1 lsl c) = 0 then c
        else loop (c+1)
      in
      color.(i) <- loop 0
    done;

    let blocks =
      proc.blocks |> Array.map begin fun blk ->
        let insts = blk.insts |> List.map (map_inst (Array.get color)) in
        { blk with insts }
      end
    in

    { proc with blocks }

end
