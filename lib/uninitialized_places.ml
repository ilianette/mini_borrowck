(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-26-27-33"]

(* You should read and understand active_borrows.ml *fully*, before filling the holes
  in this file. The analysis in this file follows the same structure. *)

open Type
open Minimir

type analysis_results = label -> PlaceSet.t

let go prog mir : analysis_results =
  (* The set of all places appearing in the MIR code. We are not interesting in initializedness for places
    which are not member of this set. *)
  let all_places =
    let acc =
      Hashtbl.fold
        (fun l _ acc -> PlaceSet.add (PlLocal l) acc)
        mir.mlocals PlaceSet.empty
    in
    Array.fold_left
      (fun acc (i, _) ->
        match i with
        | Ideinit _ | Igoto _ | Ireturn -> acc
        | Iif (pl, _, _) -> PlaceSet.add pl acc
        | Icall (_, pls, pl, _) -> PlaceSet.add_seq (Seq.cons pl (List.to_seq pls)) acc
        | Iassign (pl, rv, _) -> (
            let acc = PlaceSet.add pl acc in
            match rv with
            | RVplace pl | RVborrow (_, pl) | RVunop (_, pl) -> PlaceSet.add pl acc
            | RVbinop (_, pl1, pl2) -> PlaceSet.add pl1 (PlaceSet.add pl2 acc)
            | RVmake (_, pls) -> PlaceSet.add_seq (List.to_seq pls) acc
            | RVunit | RVconst _ -> acc))
      acc mir.minstrs
  in

  (* The set of subplaces of a given place. *)
  let subplaces = Hashtbl.create 7 in
  let () =
    PlaceSet.iter
      (fun pl ->
        let pls = PlaceSet.filter (fun pl_sub -> is_subplace pl_sub pl) all_places in
        Hashtbl.add subplaces pl pls)
      all_places
  in

  (* Effect of initializing a place [pl] on the abstract state [state]: [pl] and all its subplaces
    become initialized. Hence, given that the state is the set of uninitialized places, we remove
    the subplaces [pl] from the abstract state.

    It is important to understand that places can be nested. In order to make a place uninitialized,
    it is *not* enough to remove it from the state. One should also remove subplaces. *)
  let initialize pl state = PlaceSet.diff state (Hashtbl.find subplaces pl) in

  (* This is the dual: we are consuming or deinitiailizing place [pl], so all its subplaces
    become uninitialized, so they are added to [state]. *)
  let deinitialize pl state = PlaceSet.union state (Hashtbl.find subplaces pl) in

  (* Effect of using (copying or moving) a place [pl] on the abstract state [state]. *)
  (* task 1 *)
  let move_or_copy state pl =
    if typ_is_copy prog (typ_of_place prog mir pl)
    then state
    else deinitialize pl state
  in
  (* not task 1 anymore *)
  
  (* These modules are parameters of the [Fix.DataFlow.ForIntSegment] functor below. *)
  let module Instrs = struct let n = Array.length mir.minstrs end in
  let module Prop = struct
    type property = PlaceSet.t

    let leq_join p q = if PlaceSet.subset p q then q else PlaceSet.union p q
  end in
  let module Graph = struct
    type variable = int
    type property = PlaceSet.t

    (* To complete this module, one can read file active_borrows.ml, which contains a
      similar data flow analysis. *)

    (* DONE *)
    let foreach_root go =
      (* We assume the arguments are initialized. *)
      Hashtbl.fold (fun loc _ state ->
           match loc with Lparam _ -> initialize (PlLocal loc) state | _ -> state)
         mir.mlocals all_places
      |> go mir.mentry

    let foreach_successor lbl state go =
      match fst mir.minstrs.(lbl) with
      | Iassign(pl, rv, next) ->
         let state = initialize pl state in
          (match rv with
          | RVplace(pl') -> move_or_copy state pl' 
          | RVmake(_, pls) -> List.fold_left move_or_copy state pls 
          | _ -> state (* unary and binary operations only calculate over copy types *))
          |> go next
      | Ideinit(loc,next) -> 
         PlLocal loc |> Fun.flip deinitialize state |> go next
      | Igoto next -> go next state
      | Iif(pl, next1, next2) ->
         let state' = move_or_copy state pl in
         go next1 state';
         go next2 state'
      | Ireturn -> ()
      | Icall(_, pls, plret, next) ->
         (* After the call, we've initialized the return value and maybe uninitialized the arguments *)
         List.fold_left move_or_copy (initialize plret state) pls |> go next 
  end in
  let module Fix = Fix.DataFlow.ForIntSegment (Instrs) (Prop) (Graph) in
  fun i -> Option.value (Fix.solution i) ~default:PlaceSet.empty
