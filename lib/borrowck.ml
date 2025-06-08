open Type
open Minimir
open Active_borrows

(* This function computes the set of alive lifetimes at every program point. *)
(* Non c'est dans l'autre sens*)
let compute_lft_sets prog mir : lifetime -> PpSet.t =

  (* The [outlives] variable contains all the outlives relations between the
    lifetime variables of the function. *)
  let outlives = ref LMap.empty in

  (* Helper functions to add outlives constraints. *)
  let add_outlives (l1, l2) = outlives := add_outlives_edge l1 l2 !outlives in
  let unify_lft l1 l2 =
    add_outlives (l1, l2);
    add_outlives (l2, l1)
  in

  (* First, we add in [outlives] the constraints implied by the type of locals. *)
  Hashtbl.iter
    (fun _ typ -> outlives := outlives_union !outlives (implied_outlives prog typ))
    mir.mlocals;

    (* DONE: generate these constraints by
       - unifying types that need be equal (note that MiniRust does not support subtyping, that is,
         if a variable x: &'a i32 is used as type &'b i32, then this requires that lifetimes 'a and
         'b are equal),
       - adding constraints required by function calls,
       - generating constraints corresponding to reborrows. More precisely, if we create a borrow
         of a place that dereferences  borrows, then the lifetime of the borrow we
         create should be shorter than the lifetimes of the borrows the place dereference.
         For example, if x: &'a &'b i32, and we create a borrow y = &**x of type &'c i32,
         then 'c should be shorter than 'a and 'b.
       
    SUGGESTION: use functions [typ_of_place], [fields_types_fresh] and [fn_prototype_fresh].
     *)

  let rec unify_lft_in_typ typ1 typ2 = match typ1, typ2 with
    | Tstruct(n1, lfts1), Tstruct(n2, lfts2) when n1 = n2 ->
       List.iter2 unify_lft lfts1 lfts2
    | Tborrow(lft1,_,typ1), Tborrow(lft2,_,typ2) ->
       unify_lft lft1 lft2;
       unify_lft_in_typ typ1 typ2
    | _ -> ()
  in

  (*(in particular) if pl: 'lft T = *&'lft_big pl'
    then we should add the constraint 'lft_big : 'lft*)
  let rec beware_reborrow lft = function
    | PlDeref pl -> begin
        match typ_of_place prog mir pl with
        | Tborrow(lft_big,_,_) ->
           add_outlives (lft_big, lft); 
           beware_reborrow lft pl
        | _ -> assert false
      end
    | PlLocal _ -> ()
    | PlField (pl,_) -> beware_reborrow lft pl
  in

  (* Gives the type of an rvalue, creating lifetimes and adding constraints along the way. *)
  let approx_typ_of_rval = function
    | RVplace pl -> typ_of_place prog mir pl
    | RVconst _ | RVunit -> Tunit 
    | RVborrow(mut, pl) ->
       let t_pl = typ_of_place prog mir pl in
       let lft = fresh_lft () in

       LSet.iter (fun lft_big -> add_outlives (lft_big, lft)) (free_lfts t_pl);
       beware_reborrow lft pl;
       Tborrow(lft, mut, t_pl)
    | RVbinop _ | RVunop _ -> Tunit 
    | RVmake(name, pls) ->
       let t_fields, t_struct = fields_types_fresh prog name in
       List.iter2 unify_lft_in_typ t_fields (List.map (typ_of_place prog mir) pls);
       t_struct
  in

  Array.iter
    (fun (instr,_) -> 
      match instr with
      | Iassign(pl, rval,_) ->
         unify_lft_in_typ (typ_of_place prog mir pl) (approx_typ_of_rval rval)
      | Icall(f, pl_args, pl_ret, _) ->
         let ty_args, ty_ret, constraints = fn_prototype_fresh prog f in
         
         unify_lft_in_typ (typ_of_place prog mir pl_ret) ty_ret;
         List.iter2 unify_lft_in_typ ty_args (List.map (typ_of_place prog mir) pl_args);
         List.iter add_outlives constraints
      | _ -> ())
    mir.minstrs
  ;
    
  (* Then, we add the outlives relations needed for the instructions to be safe. *)


  (* The [living] variable contains constraints of the form "lifetime 'a should be
    alive at program point p". *)
  let living : PpSet.t LMap.t ref = ref LMap.empty in

  (* Helper function to add living constraint. *)
  let add_living pp l =
    living :=
      LMap.update l
        (fun s -> Some (PpSet.add pp (Option.value s ~default:PpSet.empty)))
        !living
  in

  (* Run the live local analysis. See module Live_locals for documentation. *)
  let live_locals = Live_locals.go mir in
  
  (* DONE: generate living constraints:
     - Add living constraints corresponding to the fact that liftimes appearing free
       in the type of live locals at some program point should be alive at that
       program point.
     - Add living constraints corresponding to the fact that generic lifetime variables
       (those in [mir.mgeneric_lfts]) should be alive during the whole execution of the
       function.
   *)
  for l = 0 to Array.length mir.minstrs -1 do
    List.iter (add_living (PpLocal l)) mir.mgeneric_lfts;
    
    let vars = live_locals l in
    LocSet.iter (fun var ->
        let t_var = typ_of_place prog mir (PlLocal var) in
        let lfts = free_lfts t_var in
        LSet.iter (add_living (PpLocal l)) lfts)
      vars
  done
  ;
  
  
  (* If [lft] is a generic lifetime, [lft] is always alive at [PpInCaller lft]. *)
  List.iter (fun lft -> add_living (PpInCaller lft) lft) mir.mgeneric_lfts;

  (* Now, we compute lifetime sets by finding the smallest solution of the constraints, using the
    Fix library. *)
  let module Fix = Fix.Fix.ForType (struct type t = lifetime end) (Fix.Prop.Set (PpSet))
  in
  Fix.lfp (fun lft lft_sets ->
      LSet.fold
        (fun lft acc -> PpSet.union (lft_sets lft) acc)
        (Option.value ~default:LSet.empty (LMap.find_opt lft !outlives))
        (Option.value ~default:PpSet.empty (LMap.find_opt lft !living)))

let borrowck prog mir =
  (* We check initializedness requirements for every instruction. *)
  let uninitialized_places = Uninitialized_places.go prog mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      let uninit : PlaceSet.t = uninitialized_places lbl in

      let check_initialized pl =
        if PlaceSet.exists (fun pluninit -> is_subplace pluninit pl) uninit then
          Error.error loc "Use of a place which is not fully initialized at this point."
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) -> (
          match pl with
          | PlDeref pl0 ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into an uninitialized borrow."
          | PlField (pl0, _) ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into a field of an uninitialized struct."
          | _ -> ())
      | _ -> ());

      match instr with
      | Iassign (_, RVplace pl, _) | Iassign (_, RVborrow (_, pl), _) ->
          check_initialized pl
      | Iassign (_, RVbinop (_, pl1, pl2), _) ->
          check_initialized pl1;
          check_initialized pl2
      | Iassign (_, RVunop (_, pl), _) | Iif (pl, _, _) -> check_initialized pl
      | Iassign (_, RVmake (_, pls), _) | Icall (_, pls, _, _) ->
          List.iter check_initialized pls
      | Ireturn -> check_initialized (PlLocal Lret)
      | Iassign (_, (RVunit | RVconst _), _) | Ideinit _ | Igoto _ -> ())
    mir.minstrs;

  (* We check the code honors the non-mutability of shared borrows. *)
  Array.iteri
    (fun _ (instr, loc) ->
      match instr with
      | Iassign(pl1, rv, _) ->
         if place_mut prog mir pl1 = NotMut
         then Error.error loc "Writing into a read-only place."
         else begin
             match rv with
             | RVborrow(Mut, pl2) ->
                if place_mut prog mir pl2 = NotMut 
                then Error.error loc "Trying to create a mutable borrow from a read-only place."
             | _ -> ()
           end
      | Ideinit _ | Igoto _ | Ireturn | Iif _ | Icall _ -> ()
    )
    mir.minstrs;

  let lft_sets = compute_lft_sets prog mir in

  (* DONE: check that outlives constraints declared in the prototype of the function are
    enough to ensure safety. I.e., if [lft_sets lft] contains program point [PpInCaller lft'], this
    means that we need that [lft] be alive when [lft'] dies, i.e., [lft'] outlives [lft]. This relation
    has to be declared in [mir.outlives_graph]. *)

  (* message d'erreur piqué à Alexis. Merci ! *)
  let err lft_big lft =
  Error.error mir.mloc
    "Explicit outlive relation '%s : %s' not provided in function signature"
      (match lft_big with Ast.Lnamed s -> s | _ -> "")
      (match lft with Ast.Lnamed s -> s | _ -> "")
  in
  
  let check lft =
    let constraints = LMap.find lft mir.moutlives_graph in
    PpSet.iter (function
        | PpInCaller lft_big ->
           if not (LSet.mem lft_big constraints) then
             err lft_big lft
        | _ -> ()) (lft_sets lft)
  in

  List.iter check mir.mgeneric_lfts;
  
  (* We check that we never perform any operation which would conflict with an existing
    borrows. *)
  let bor_active_at = Active_borrows.go prog lft_sets mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      (* The list of bor_info for borrows active at this instruction. *)
      let active_borrows_info : bor_info list =
        List.map (get_bor_info prog mir) (BSet.to_list (bor_active_at lbl))
      in

      (* Does there exist a borrow of a place pl', which is active at program point [lbl],
        such that a *write* to [pl] conflicts with this borrow?

         If [pl] is a subplace of pl', then writing to [pl] is always conflicting, because
        it is aliasing with the borrow of pl'.

         If pl' is a subplace of [pl], the situation is more complex:
           - if pl' involves as many dereferences as [pl] (e.g., writing to [x.f1] while
            [x.f1.f2] is borrowed), then the write to [pl] will overwrite pl', hence this is
            conflicting.
           - BUT, if pl' involves more dereferences than [pl] (e.g., writing to [x.f1] while
            [*x.f1.f2] is borrowed), then writing to [pl] will *not* modify values accessible
            from pl'. Hence, subtlely, this is not a conflict. *)
      let conflicting_borrow_no_deref pl =
        List.exists
          (fun bi -> is_subplace pl bi.bplace || is_subplace_no_deref bi.bplace pl)
          active_borrows_info
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) ->
          if conflicting_borrow_no_deref pl then
            Error.error loc "Assigning a borrowed place."
      | Ideinit (l, _) ->
          if conflicting_borrow_no_deref (PlLocal l) then
            Error.error loc
              "A local declared here leaves its scope while still being borrowed."
      | Ireturn ->
          Hashtbl.iter
            (fun l _ ->
              match l with
              | Lparam p ->
                  if conflicting_borrow_no_deref (PlLocal l) then
                    Error.error loc
                      "When returning from this function, parameter `%s` is still \
                       borrowed."
                      p
              | _ -> ())
            mir.mlocals
      | _ -> ());

      (* Variant of [conflicting_borrow_no_deref]: does there exist a borrow of a place pl',
        which is active at program point [lbl], such that a *read* to [pl] conflicts with this
        borrow? In addition, if parameter [write] is true, we consider an operation which is
        both a read and a write. *)
      let conflicting_borrow write pl =
        List.exists
          (fun bi ->
            (bi.bmut = Mut || write)
            && (is_subplace pl bi.bplace || is_subplace bi.bplace pl))
          active_borrows_info
      in

      (* Check a "use" (copy or move) of place [pl]. *)
      let check_use pl =
        let consumes = not (typ_is_copy prog (typ_of_place prog mir pl)) in
        if conflicting_borrow consumes pl then
          Error.error loc "A borrow conflicts with the use of this place.";
        if consumes && contains_deref_borrow pl then
          Error.error loc "Moving a value out of a borrow."
      in

      match instr with
      | Iassign (_, RVplace pl,_) | Iassign (_, RVunop (_, pl), _) -> check_use pl
      | Iassign (_, RVbinop (_, pl1, pl2), _) ->
         check_use pl1;
         check_use pl2;
      | Iassign (_, RVmake (_, pls), _) -> List.iter check_use pls
      | Iassign (_, RVborrow (mut, pl), _) ->
          if conflicting_borrow (mut = Mut) pl then
            Error.error loc "There is a borrow conflicting this borrow."
      | Iassign _ -> ()
     
      | Ideinit _  | Ireturn  | Igoto _ -> ()
     
      | Iif(pl, _, _) -> check_use pl
      | Icall(_, pls, _, _) ->  List.iter check_use pls
 
    )
    mir.minstrs
