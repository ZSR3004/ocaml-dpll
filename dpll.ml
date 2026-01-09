type literal =
  | Pos of string
  | Neg of string
[@@deriving show {with_path=false}]

type clause = literal list
[@@deriving show {with_path=false}]

type cnf = clause list
[@@deriving show {with_path=false}]

type assignment = (string * bool) list
[@@deriving show {with_path=false}]


let subset (l1 : 'a list) (l2 : 'a list) : bool =
  (* [subset l1 l2] checks if every member of l1 is a member of l2 *)
  List.for_all (fun a -> List.mem a l2) l1

let rec absorb (f : cnf) : cnf = 
  (* [absorb f] removes redundant clauses from f, producing a
     logically equivalent, irredundant formula *)
  let rec filter_clause (c : clause) (f : cnf) (irr_f : cnf) : (clause * cnf) =
    (* [filter_clause c f] removes all supersets of c
       and keeps only the smallest subset of c. *)
    match f with
    | [] -> (c, List.rev irr_f)
    | c'::f' -> if subset c' c then filter_clause c' f' irr_f
                  else if subset c c' then filter_clause c f' irr_f
                  else filter_clause c f' (c'::irr_f)
  in

  match f with
  | [] -> []
  | c::f' ->  let (subset_c, absorbed_f'') = filter_clause c f' [] in
              subset_c::(absorb absorbed_f'')


let cnf_taut (f : cnf) : bool =
  (* [cnf_taut f] efficiently determines whether or not f is a
     tautology *)

  let negate_literal (l : literal) : literal =
    (* [negate literal l] Negates a literal l. *)
    match l with
    | Pos x -> Neg x
    | Neg x -> Pos x
  in

  let rec cnf_clause_taut (c : clause) : bool =
    (* [cnf_clause_taut c] checks if c is a tautology
       by checking for a literal complement pair.*)
    match c with
    | [] -> false
    | l::c' -> List.mem (negate_literal l) (c) || cnf_clause_taut c'
  in
  
  List.for_all (fun c -> cnf_clause_taut c) (f)

let%test_module "cnfset" =
  (module struct
    
    let%test _ = subset [] [3 ; 1 ; 2]
    let%test _ = subset [1 ; 2 ; 2] [3 ; 1 ; 2]
    let%test _ = not (subset [4] [3 ; 1 ; 2])
    let%test _ = not (subset [3 ; 4] [3 ; 1 ; 2])
    
    (* ==== MY ABSORB TESTS === *)
    let%test _ = absorb [] = []
    let%test _ = absorb [[]] = [[]]
    let%test _ = absorb [[]; []] = [[]]
    let%test _ = absorb [[]; [Pos "x"]; [Neg "y"]] = [[]]
    let%test _ = absorb [[Pos "x"]; [Pos "x"]] = [[Pos "x"]]
    let%test _ = absorb [[Pos "x"; Neg "x"]; [Pos "x"]] = [[Pos "x"]]
    let%test _ = absorb [[Pos "x"; Pos "y"]; [Pos "x"]] = [[Pos "x"]]
    let%test _ = absorb [[Pos "x"]; [Neg "x"]] = [[Pos "x"]; [Neg "x"]]
    let%test _ = absorb [[Pos "x"; Pos "y"]; [Pos "z"]] = [[Pos "x"; Pos "y"]; [Pos "z"]]
    let%test _ = absorb [[Pos "a"; Pos "b"]; [Pos "c"]] = [[Pos "a"; Pos "b"]; [Pos "c"]]
    let%test _ = absorb [[Pos "x"; Neg "y"]; [Neg "y"]; [Pos "x"]] = [[Neg "y"]; [Pos "x"]]
    let%test _ = absorb [[Neg "y"]; [Pos "x"; Neg "y"]; [Pos "x"]] = [[Neg "y"]; [Pos "x"]]
    let%test _ = absorb [[Pos "x"]; [Pos "y"]; [Neg "z"]] = [[Pos "x"]; [Pos "y"]; [Neg "z"]]
    let%test _ = absorb [[Pos "x"; Pos "y"; Pos "z"]; [Pos "x"; Pos "y"]] = [[Pos "x"; Pos "y"]]
    let%test _ = absorb [[Pos "x"; Pos "y"]; [Pos "x"; Pos "y"; Pos "z"]] = [[Pos "x"; Pos "y"]]
    let%test _ = absorb [[Pos "x"]; [Pos "x"; Pos "y"]; [Pos "x"; Pos "y"; Pos "z"]] = [[Pos "x"]]
    let%test _ = absorb [[Neg "y"]; [Pos "x"; Neg "y"]; [Pos "x"; Pos "z"; Pos "w"]] = [[Neg "y"]; [Pos "x"; Pos "z"; Pos "w"]]
    let%test _ = absorb [[Pos "x"; Pos "y"]; [Pos "x"]; [Pos "y"]; [Pos "z"; Pos "w"]] = [[Pos "x"]; [Pos "y"]; [Pos "z"; Pos "w"]]
    let%test _ = absorb [[Pos "x"; Neg "y"]; [Pos "y"; Pos "z"]; [Neg "z"]; [Pos "w"]] = [[Pos "x"; Neg "y"]; [Pos "y"; Pos "z"]; [Neg "z"]; [Pos "w"]]


    (* === MY CNF_TAUT TESTS === *)
    let%test _ = cnf_taut [] = true
    let%test _ = cnf_taut [[]] = false
    let%test _ = cnf_taut [[]; []] = false
    let%test _ = cnf_taut [[Pos "x"]] = false 
    let%test _ = cnf_taut [[Pos "x"; Neg "x"]] = true
    let%test _ = cnf_taut [[Neg "x"; Neg "x"]] = false
    let%test _ = cnf_taut [[Pos "x"]; [Neg "x"]] = false
    let%test _ = cnf_taut [[Pos "x"; Neg "x"]; []] = false
    let%test _ = cnf_taut [[Pos "x"; Neg "x"; Pos "y"]] = true
    let%test _ = cnf_taut [[Pos "a"; Neg "a"; Pos "c"]] = true
    let%test _ = cnf_taut [[Pos "x"; Neg "y"; Pos "y"]] = true
    let%test _ = cnf_taut [[Neg "x"; Neg "x"; Neg "y"]] = false
    let%test _ = cnf_taut [[Pos "x"; Pos "y"; Pos "z"]] = false
    let%test _ = cnf_taut [[Pos "x"; Neg "x"]; [Pos "y"; Neg "y"]] = true
    let%test _ = cnf_taut [[Pos "x"; Pos "x"]; [Pos "x"; Neg "x"]] = false 
    let%test _ = cnf_taut [[Neg "x"; Pos "y"]; [Pos "x"; Neg "y"]] = false
    let%test _ = cnf_taut [[Pos "x"; Pos "y"]; [Neg "y"; Neg "x"]] = false
    let%test _ = cnf_taut [[Pos "p"; Neg "q"]; [Neg "p"; Pos "q"]] = false
    let%test _ = cnf_taut [[Pos "x"; Pos "y"; Neg "y"]; [Neg "x"; Neg "y"; Pos "y"]] = true
    let%test _ = cnf_taut [[Pos "x"; Neg "x"; Pos "y"]; [Neg "y"; Pos "y"; Pos "z"]] = true

  end)
