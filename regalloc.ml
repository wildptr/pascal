open Batteries
open Ir
open Inst

module S = Set.Int

exception Break

type color_constraint =
  | Distinct of int list
  | Same of int

module Machine (M : MachineType) = struct

  module MIR = OfInstType(M.SpecInst)
  module MInst = Inst.Machine(M)

  open MIR
  open MInst

  let allocate_registers proc =

    (* liveness analysis *)

    let blocks = proc.blocks in
    let n = Array.length blocks in
    let nr = proc.n_reg in

    let def       = Array.make n S.empty in
    let use       = Array.make n S.empty in
    let live_in   = Array.make n S.empty in
    let live_out  = Array.make n S.empty in

    let pred = Array.make n [] in

    for i=0 to n-1 do
      blocks.(i).insts |> List.iter begin fun inst ->
        use.(i) <- S.union use.(i) (S.diff (uses inst) def.(i));
        def.(i) <- S.union def.(i) (defs inst)
      end;
      blocks.(i).succ |> List.iter (fun j -> pred.(j) <- i :: pred.(j))
    done;

    let changed = ref false in
    let rec loop it =
      for i=n-1 downto 0 do
        live_in.(i) <- S.union use.(i) (S.diff live_out.(i) def.(i));
        pred.(i) |> List.iter begin fun j ->
          let tmp = S.union live_out.(j) live_in.(i) in
          if not (S.equal tmp live_out.(j)) then
            (changed := true; live_out.(j) <- tmp)
        end;
      done;
      if !changed then
        (changed := false; loop (it+1))
      else
        Printf.eprintf "liveness(%s): fixpoint reached after iteration %d\n"
          proc.name (it+1)
    in
    loop 0;

    (* build interference graph *)

    let module G =
      Graph.Imperative.Graph.Concrete
        (struct
          type t = int
          let compare = Int.compare
          let equal = Int.equal
          let hash i = i
        end)
    in

    let g = G.create ~size:nr () in
    let m = G.create ~size:nr () in
    for i=0 to nr-1 do
      G.add_vertex g i;
      G.add_vertex m i
    done;

    let connect defs live =
      let a = S.to_array live in
      let n = Array.length a in
      for i=0 to n-1 do
        for j=i+1 to n-1 do
          (* Printf.eprintf "interfere: %d -- %d\n" a.(i) a.(j); *)
          G.add_edge g a.(i) a.(j)
        done
      done
    in

    for i=0 to n-1 do
      let live_in_i =
        List.fold_right begin fun inst live ->
          let def = defs inst in
          connect def live;
          begin match inst with
            | MOV (d, Reg s) ->
              Printf.eprintf "move-related: %d -- %d\n" d s;
              G.add_edge m d s
            | _ -> ()
          end;
          S.union (S.diff live def) (uses inst)
        end blocks.(i).insts live_out.(i)
      in
      assert (S.equal live_in_i live_in.(i));
      pred.(i) |> List.iter begin fun j ->
        let def = defs (List.last blocks.(j).insts) in
        connect def live_in_i
      end
    done;

    (* register allocation *)

    let move_related i =
      G.out_degree m i > 0
    in

    let low_degree i =
      G.out_degree g i < M.n_reg_avail
    in

    let color_order = Array.make nr (0, Distinct []) in
    begin
      let p = ref nr in
      let nodes = ref Set.Int.empty in
      for i=0 to nr-1 do
        nodes := Set.Int.add i !nodes
      done;
      let remove_node i c =
        decr p;
        color_order.(!p) <- (i, c);
        G.remove_vertex g i;
        G.remove_vertex m i;
        nodes := Set.Int.remove i !nodes
      in
      (* simplify *)
      let rec simplify () =
        let old_p = !p in
        !nodes |> Set.Int.iter begin fun i ->
          if low_degree i && not (move_related i) then
            remove_node i (Distinct (G.succ g i))
        end;
        if !p < old_p then simplify ()
      in
      (* coalesce *)
      (* George's strategy:
         Nodes i and j can be coalesced if, for every neighbor k of i, either k
         already interferes with j or k is of insignificant degree. *)
      let coalesce () =
        let changed = ref false in
        let rec loop () =
          try
            G.iter_edges begin fun i j ->
              begin
                try
                  G.iter_succ begin fun k ->
                    if not (G.mem_edge g k j || low_degree k) then
                      raise Break
                  end g i;
                with Break -> () (* cannot coalesce *)
              end;
              Printf.printf "coalesce %d %d\n" i j;
              remove_node j (Same i);
              changed := true;
              raise Break
            end m;
          with Break -> loop ()
        in
        loop ();
        !changed
      in
      let freeze () =
        try
          G.iter_vertex begin fun i ->
            if move_related i && low_degree i then begin
              Printf.eprintf "freeze %d\n" i;
              G.iter_succ (G.remove_edge g i) g i;
              raise Break
            end
          end g;
          false
        with Break -> true
      in
      let spill () =
        let i = Set.Int.choose !nodes in
        remove_node i (Distinct (G.succ g i))
      in
      let rec loop () =
        simplify ();
        if !p > 0 then begin
          if coalesce () then loop ();
          if freeze () then loop ();
          if !p > 0 then begin
            spill ();
            if !p > 0 then loop ()
          end
        end
      in
      loop ()
    end;

    let color = Array.make nr (-1) in
    (* pre-color physical registers *)
    for i=0 to M.n_reg-1 do color.(i) <- i done;
    color_order |> Array.iter begin fun (i, c) ->
      match c with
      | Distinct l ->
        let f =
          List.fold_left (fun f j -> f lor (1 lsl color.(j)))
            M.reg_mask l
        in
        let rec loop c =
          if f land (1 lsl c) = 0 then c
          else loop (c+1)
        in
        color.(i) <- loop 0
      | Same j ->
        assert (color.(j) >= 0);
        color.(i) <- color.(j)
    end;

    let blocks =
      proc.blocks |> Array.map begin fun blk ->
        let insts = blk.insts |> List.map (map_inst (Array.get color)) in
        { blk with insts }
      end
    in

    { proc with blocks }

end
