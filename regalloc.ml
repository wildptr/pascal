open Batteries
open Ir

module S = Set.Int

exception Break

type color_constraint =
  | Distinct of int list
  | Same of int

module Make (M : MachineType) = struct

  module MIR = MakeIR(MakeInst(M))
  open MIR

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
        use.(i) <- S.union use.(i) (S.diff (M.uses inst) def.(i));
        def.(i) <- S.union def.(i) (M.defs inst)
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
          G.add_edge g a.(i) a.(j)
        done
      done
    in

    let pred_insts i =
      let rec loop s =
        let s' =
          Set.Int.fold
            (fun i s ->
               let insts = blocks.(i).insts in
               if insts = [] then
                 S.union s (Set.Int.of_list pred.(i))
               else s)
            s s
        in
        if not (Set.Int.equal s s') then loop s' else s'
      in
      S.fold
        (fun i l ->
           let insts = blocks.(i).insts in
           if insts = [] then l else List.last insts :: l)
        (loop (S.singleton i))
        []
    in

    for i=0 to n-1 do
      let live_in_i =
        List.fold_right begin fun inst live ->
          let def = M.defs inst in
          connect def live;
          M.move_related_pairs inst |> List.iter
            (fun (r1, r2) -> if r1 <> r2 then G.add_edge m r1 r2);
          S.union (S.diff live def) (M.uses inst)
        end blocks.(i).insts live_out.(i)
      in
      assert (S.equal live_in_i live_in.(i));
      pred_insts i |> List.iter
        (fun inst -> connect (M.defs inst) live_in_i)
    done;

    G.iter_edges (Printf.eprintf "interfere: %d -- %d\n") g;
    G.iter_edges (Printf.eprintf "move-related: %d -- %d\n") m;

    (* register allocation *)

    let move_related i =
      G.out_degree m i > 0
    in

    let low_degree i =
      G.out_degree g i < M.n_reg_avail
    in

    let nvirt = nr - M.n_reg in
    let color_order = Array.make nvirt (0, Distinct []) in
    begin
      let p = ref nvirt in
      let nodes = ref Set.Int.empty in
      for i=M.n_reg to nr-1 do
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
          if low_degree i && not (move_related i) then begin
            Printf.eprintf "simplify %d\n" i;
            remove_node i (Distinct (G.succ g i))
          end
        end;
        if !p < old_p then simplify ()
      in
      (* coalesce *)
      (* George's strategy:
         Nodes i and j can be coalesced if, for every neighbor k of i, either k
         already interferes with j or k is of insignificant degree. *)
      let coalesce () =
        let old_p = !p in
        let rec loop () =
          try
            G.iter_edges begin fun i j ->
              if not (G.mem_edge g i j) then begin
                let i, j =
                  if i<j then i, j else j, i
                in
                if j >= M.n_reg then begin
                  begin
                    try
                      G.iter_succ begin fun k ->
                        if not (G.mem_edge g k j || low_degree k) then
                          raise Break
                      end g i;
                    with Break -> () (* cannot coalesce *)
                  end;
                  Printf.printf "coalesce %d %d\n" i j;
                  G.iter_succ (G.add_edge g i) g j;
                  remove_node j (Same i);
                  raise Break
                end
              end
            end m;
          with Break -> loop ()
        in
        loop ();
        !p < old_p
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
        Printf.eprintf "spill %d\n" i;
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
          List.fold_left
            (fun f j ->
               let c = color.(j) in
               assert (c >= 0);
               f lor (1 lsl c))
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

    for i=M.n_reg to nr-1 do
      Printf.eprintf "color[%d]=%d\n" i color.(i)
    done;

    let blocks =
      proc.blocks |> Array.map begin fun blk ->
        let insts = blk.insts |> List.map (M.map_inst (Array.get color)) in
        { blk with insts }
      end
    in

    { proc with blocks }

end
