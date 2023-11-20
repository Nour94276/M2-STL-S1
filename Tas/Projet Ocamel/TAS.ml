module MM = Map.Make (String);;
module SM = Set.Make(String);;

(* Terms *)
type pterm = Var of string    (* variable *)
  | App of pterm * pterm      (* abstraction *)
  | Abs of string * pterm     (* application *)
  | Int of int                 
  | Add of pterm * pterm       (* addition *)  
  | Sub of pterm * pterm       (* soustration *)
  | List of pterm list         (* list *)
  | Tete of pterm              (* La tête de la liste *)
  | Queue of pterm             (* La queue de la liste *)
  | Cons of pterm * pterm      (* Construire de la liste*)
  | IFZERO of pterm * pterm * pterm   (* Si 0 renvoie la première expression, sinon renvoie la deuxième expression *)
  | IFEMPTY of pterm * pterm * pterm  (* Si la condition est vided, renvoie la première expression, sinon renvoie la deuxième expression *)
  | Fix of string * pterm                     (* point fix qui défini une fonction récursive *)
  | Let of string * pterm *pterm      (* let *)
  | Ref of pterm ref           (* La création de région *)
  | DeRef of pterm             (* le déréferencement *)
  | Assign of pterm * pterm    (* L’assignement *)
  | Unit                       (* une valeur unit() *)
;;
(* pretty printer for terms *)
let rec print_term (pt : pterm) : string =
  match pt with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
    | Int n -> string_of_int n  
    | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
    | Sub (t1, t2) -> "(" ^ (print_term t1) ^" - "^ (print_term t2) ^ ")"
    | List l -> "[" ^ String.concat "," (List.map print_term l) ^ "]"
    | Tete t -> "(tete " ^ (print_term t) ^ ")"
    | Queue q ->"(queue " ^ (print_term q) ^ ")"
    | Cons (c,l) ->"(cons " ^ print_term c ^ " " ^ print_term l ^ ")"
    | IFZERO (c,e1,e2) -> "(ifzero " ^ print_term c ^ " then " ^ print_term e1 ^ " else " ^ print_term e2 ^ ")"
    | IFEMPTY (c,e1,e2) -> "(ifempty " ^ print_term c ^ " then " ^ print_term e1 ^ " else " ^ print_term e2 ^ ")"
    | Fix (phi,m) -> "(fix (" ^ phi ^ "," ^ print_term m  ^ ")"
    | Let (v,e1,e2) -> "(let " ^ v ^ " = " ^ print_term e1 ^ " in " ^ print_term e2 ^ ")"
    | Ref r -> "(ref " ^ print_term !r ^ ")"
    | DeRef d -> "(!" ^ print_term d ^ ")"
    | Assign(e1,e2) -> "(" ^ print_term e1 ^ ":=" ^ print_term e2 ^ ")"
    | Unit -> "unit()"
  ;;

  let createVar v = Var v;;
  let createAbs v t = Abs(v,t);;
  let createApp m n = App(m,n);;
  let createInt i = Int i;;
  let createAdd t1 t2 = Add(t1,t2);;
  let createSub t1 t2 = Sub(t1,t2);;
  let createList l = List l;;
  let createTete t = Tete t;;
  let createQueue q = Queue q;;
  let createCons c l = Cons(c,l);;
  let createIFZERO c e1 e2 = IFZERO(c,e1,e2);;
  let createIFEMPTY c e1 e2 = IFEMPTY(c,e1,e2);;
  let createFix f x = Fix (f,x);;
  let createLet v e1 e2 = Let(v,e1,e2);;
  let createRef r = Ref (ref r);;
  let createDeRef d = DeRef d;;
  let createAssign e1 e2 = Assign(e1,e2);;
  let createUnit = Unit;;

let compteur_var : int ref = ref 0   
let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
  "T"^(string_of_int !compteur_var)
(* Replace bound variables with other new variables *)
let rec substitue_name p new_value old_value =
  match p with
    Var v -> if v = old_value then Var new_value else p
  | App(v1,v2)  -> App(substitue_name v1 new_value old_value, substitue_name v2 new_value old_value)
  | Abs(v1, v2) -> if v1 = old_value then Abs(new_value,substitue_name v2 new_value old_value)
      else Abs(v1,substitue_name v2 new_value old_value)
  | _ -> p
;;

let rec alpha_convertit (p:pterm) = 
  match p with
    Var v -> Var v
  | App(p1,p2) -> App(alpha_convertit p1,alpha_convertit p2)
  | Abs(p1,p2) -> 
      let n_name = nouvelle_var() in 
      (Abs(n_name, substitue_name (alpha_convertit p2) n_name p1))
  | Let(v,e1,e2) -> Let(v,alpha_convertit e1,alpha_convertit e2)
  |_ -> p
;;

let rec sub_var old_term variable new_term = 
  match old_term with
    Var v -> if v = variable then new_term else old_term 
  | App(p1,p2) -> App(sub_var p1 variable new_term,sub_var p2 variable new_term )
  | Abs(p1,p2) -> if p1 = variable then old_term
                     else (Abs(p1, sub_var p2 variable new_term))
  | Add(p1,p2) -> Add(sub_var p1 variable new_term,sub_var p2 variable new_term)
  | Sub(p1,p2) -> Sub(sub_var p1 variable new_term,sub_var p2 variable new_term)
  | _ -> old_term
;;

let evaluation term = 
  match term with
    Var v -> Var v
  | Abs(v1,v2) -> Abs(v1,v2)
  | App(v1,v2) -> (match v1 with
                  Var v -> Var v
                | Abs(t1,t2) -> sub_var v1 t1 v2 
                | App(t1,t2) -> term
                |_ -> term)
  |_ -> term
;;
exception WrongType of string;;

let max_time = 5.0;;
exception Timeout of string;;

let rec reduce (e : pterm) =
  let start = Sys.time () in
  let rec reduit_t expr =
    if Sys.time () -. start < max_time then
      match expr with
      | App (Abs (v1, v2), e2) -> sub_var v2 v1 e2
      | App (e1, e2) ->
          let v1 = reduit_t e1 in
          if v1 != e1 then
            reduit_t (App (v1, e2))
          else
            App (e1, reduit_t e2)
      | Abs (x, e) -> Abs (x, reduit_t e)
      | Var x -> Var x
      | Int x -> Int x
      | Add (x, y) -> (
          let a = reduit_t x and b = reduit_t y in
          match (a, b) with
          | Int x1, Int x2 -> Int (x1 + x2)
          | Var _, Var _ -> Add (x, y)
          | Var _, Int _ -> Add (x, y)
          | Int _, Var _ -> Add (x, y)
          | _ -> raise (WrongType ("Wrong type add"))
        )
      | Sub (x, y) -> (
          let a = reduit_t x and b = reduit_t y in
          match (a, b) with
          | Int x1, Int x2 -> Int (x1 - x2)
          | _ -> raise (WrongType ("Wrong type sub"))
        )
      | Tete t -> (
          match t with
          | List l -> List.hd l
          | _ -> raise (WrongType ("Wrong type"))
        )
      | Queue q -> (
          match q with
          | List l -> List (List.tl l)
          | _ -> raise (WrongType ("Wrong type"))
        )
      | Cons (x, y) -> (
          match (x, y) with
          | a, List l -> List (a :: l)
          | _ -> raise (WrongType ("Wrong type"))
        )
      | IFZERO (c, e1, e2) -> (
          let r = reduit_t c in
          match r with
          | Int a -> if a = 0 then reduit_t e1 else reduit_t e2
          | _ -> raise (WrongType ("Wrong type"))
        )
      | IFEMPTY (c, e1, e2) -> (
          match c with
          | List l -> (
              match l with
              | [] -> reduit_t e1
              | _ -> reduit_t e2
            )
          | _ -> raise (WrongType ("Wrong type"))
        )
      | Fix (x, y) -> sub_var y x (Fix (x, y))
      | Let (v, e1, e2) -> reduit_t (sub_var e2 v (reduit_t e1))
      | _ -> expr
    else
      raise (Timeout ("timeout"))
  in
  reduit_t e


(* Types *)
type ptype = 
    Var of string 
  | Arr of ptype * ptype 
  | Nat 
  | Liste of ptype 
  | All of SM.t * ptype
  | Unit
  | Ref of ptype
;;
let createVarType t = Var t;;
let createArrType t1 t2 = Arr(t1,t2);;
(* 3.3.3 *)  
let createNatType t = Nat;;
let createListType t = Liste t;;
let createAllType e1 e2 = All(e1,e2);;
(* 4.3.3 *)
let createUnitType = Unit;;
let createRefType r = Ref r;;
(* pretty printer for types *)
let rec print_type (t : ptype) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | Nat -> "Nat" 
  | Liste t -> "[" ^ print_type t ^"]"
  | All (v,ty) -> "∀" ^ (String.concat "," (SM.elements v)) ^ "." ^ print_type ty
  | Unit -> "unit"
  | Ref r -> "ref " ^ print_type r 
;;
(* Typing environments *)
type env = (string * ptype) list 
(* Lists of equations *)
type equa = (ptype * ptype) list

exception VarIntrouvable
exception RuntimeError of string;;
(* Fresh variable names generator for types *)
let compteur_var : int ref = ref 0                    

let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
  "T"^(string_of_int !compteur_var)

(* Looks up the type of a variable in an environment *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarIntrouvable
  | (v1, t1)::q when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q) 

(* Generates typing equations from a term *)
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
  | Var v ->
      let tv : ptype = cherche_type v e in
      [(ty, tv)]
  | App (t1, t2) ->
      let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) ->
      let nv1 : string = nouvelle_var () in
      let nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2)) :: (genere_equa t (Var nv2) ((x, Var nv1) :: e))
  | Int _ -> [(ty, Nat)]
  | Add (t1, t2) | Sub (t1, t2) ->
      let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat) :: (eq1 @ eq2)
  | List t ->
      let nv1 : string = nouvelle_var () in
      (ty, Liste (Var nv1)) :: (List.concat (List.map (fun f -> genere_equa f (Var nv1) e) t))
  | Tete t | Queue t ->
      let nv1 : string = nouvelle_var () in
      (ty, Var nv1) :: (genere_equa t (Liste (Var nv1)) e)
  | Cons (t1, t2) ->
      let nv1 = nouvelle_var () in
      let l1 = genere_equa t1 (Var nv1) e in
      let l2 = genere_equa t2 (Liste (Var nv1)) e in
      (ty, Liste (Var nv1)) :: (l1 @ l2)
  | IFZERO (c, e1, e2) ->
      let eq1 = genere_equa c Nat e in
      let eq2 = genere_equa e1 ty e in
      let eq3 = genere_equa e2 ty e in
      eq1 @ (eq2 @ eq3)
  | IFEMPTY (c, e1, e2) ->
      let nv = nouvelle_var () in
      let eq1 = genere_equa c (All (SM.singleton (print_type (Var nv)), Liste (Var nv))) e in
      let eq2 = genere_equa e1 ty e in
      let eq3 = genere_equa e2 ty e in
      eq1 @ (eq2 @ eq3)
  | Ref r ->
      let nv = nouvelle_var () in
      (ty, Ref (Var nv)) :: (genere_equa !r (Var nv) e)
  | DeRef d ->
      let nv = nouvelle_var () in
      (ty, Var nv) :: (genere_equa d (Ref (Var nv)) e)
  | Assign (e1, e2) ->
      let nv = nouvelle_var () in
      let eq1 = genere_equa e1 (Ref (Var nv)) e in
      let eq2 = genere_equa e2 (Var nv) e in
      (ty, Unit) :: (eq1 @ eq2)
  | _ -> raise (RuntimeError ("ERROR : Can't find this type"))


let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2) 
  | Nat -> false
  | Liste l -> appartient_type v l
  | All(var,tp) -> if not (SM.mem v var) then appartient_type v tp else false
  | _ -> false

let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr(t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0) 
  | Nat -> Nat 
  | Liste l -> Liste (substitue_type l v t0)
  | All(var,ty) -> if not (SM.mem v var) then All(var,substitue_type ty v t0) else t
  | Unit -> Unit
  | Ref r -> Ref (substitue_type r v t0)
;;   

(* replaces a variable with a type in a list of equations *)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e
;;

let apply_barendregt var ty =
  let new_type_ref = ref ty in
  SM.iter (fun subst ->
    let new_var = Var (nouvelle_var ()) in
    new_type_ref := substitue_type !new_type_ref subst new_var
  ) var;
  !new_type_ref
;;


exception Echec_unif of string      

(* zipper *)
type equa_zip = equa * equa
  
let rec rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* substitutes a variable with a type in an equation zipper *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : ptype) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* finds a type associated with a variable in an equation zipper *)
let rec trouve_but (e : equa_zip) (but : string) = 
  match e with
    (_, []) -> raise VarIntrouvable
  | (_, (Var v, t)::_) when v = but -> t
  | (_, (t, Var v)::_) when v = but -> t 
  | (e1, c::e2) -> trouve_but (c::e1, e2) but 

(* solves a system of equations *)
let rec unification (equations : equa_zip) (goal : string) : ptype = 
  match equations with 
    (_, []) -> (try trouve_but (rembobine equations) goal with VarIntrouvable -> raise (Echec_unif "but pas trouvé"))
  | (e1, (Var v1, t2)::e2) when v1 = goal ->  unification ((Var v1, t2)::e1, e2) goal
  | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) goal
  | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurrence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) goal
  | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurrence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) goal 
  | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) goal 
  | (e1, (Arr (_,_), t3)::e2) -> raise (Echec_unif ("type flèche non-unifiable avec "^(print_type t3)))     
  | (e1, (t3, Arr (_,_))::e2) -> raise (Echec_unif ("type flèche non-unifiable avec "^(print_type t3)))     
  | (e1, (Nat, Nat)::e2) -> unification (e1, e2) goal 
  | (e1, (Nat, t3)::e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))     
  | (e1, (t3, Nat)::e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))     
(* exercice 3.5 *) 
    (* for all types, applique "apply_barendregt" *)
  | (e1, (All(v1,t1), All(v2, t2))::e2) -> unification (e1, ((apply_barendregt v1 t1),(apply_barendregt v2 t2))::e2) goal 
  | (e1, (All(v1,t1), t2)::e2) -> unification (e1, ((apply_barendregt v1 t1),t2)::e2) goal 
  | (e1, (t1, All(v2, t2))::e2) -> unification (e1, (t1,(apply_barendregt v2 t2))::e2) goal 
  | (e1, (t1, t2)::e2) -> raise (Echec_unif ("Les types ne sont pas les mêmes "))
  | (e1,(Ref t1,Ref t2)::e2) -> unification (e1,(t1,t2)::e2) goal
                                 
let inference (t : pterm) : string =
  let e : equa_zip = ([], genere_equa t (Var "but") []) in
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla

  
    let identity_function : pterm = Abs ("x", Var "x") 
    let reduced_identity  = reduce identity_function
    let inferred_identity : string = inference identity_function
  
    let k_function : pterm = Abs ("x", Abs ("y", Var "x"))
    let reduced_k = reduce k_function
    let inferred_k : string = inference k_function
  
    let k_composed_with_identity : pterm = App (k_function, identity_function)
    let alpha_converted_ki = alpha_convertit k_composed_with_identity
    let reduced_ki = reduce alpha_converted_ki
    let inferred_ki = inference reduced_ki
    let k_composed_twice_with_identity : pterm = App (App (k_function, identity_function), identity_function)
    let alpha_converted_kii = alpha_convertit k_composed_twice_with_identity
    let reduced_kii = reduce alpha_converted_kii
    let inferred_kii = inference reduced_kii
    let s_function : pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
    let inferred_s : string = inference s_function
    (* nat_function = (λx.(x+1))*5 *)
    let nat_function : pterm = App (Abs ("x", Add (Var "x", Int 1)), Int 5)
    let inferred_nat : string = inference nat_function
     (* nat1_function = λx.(x+x) *)
    let nat1_function : pterm = Abs ("x", Add (Var "x", Var "x"))
    let reduced_nat1 = reduce nat1_function
    let inferred_nat1 : string = inference nat1_function
    (* omega_function = (λx.xx)(λy.yy) *)
    let omega_function : pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
    let reduced_omega = reduce omega_function
    let inferred_omega : string = inference omega_function

    let ex_ifempty = IFEMPTY(Tete (List [Int 10]),Int 3, Tete(List []))
    let inferred_ifempty = inference ex_ifempty

    let ex_ref = IFZERO(Ref (ref (Int 3)),Int 2,Int 1)
    let inferred_ex_ref = inference ex_ref

    let r = App(Abs("x",Var "x"),Ref (ref (Int 4)))
    let inferred_r = inference r
    (* nat3_function = nat1_function * identity_function *)
    let nat3_function : pterm = App (nat1_function, identity_function)
    let reduced_nat3 = reduce nat3_function
    let inferred_nat3 : string = inference nat3_function

    let ex_ifzero = IFZERO (Sub (Int 10, Int 3), Int 4, Int 1)
    let reduced_ifzero = reduce ex_ifzero
    let inferred_ifzero = inference ex_ifzero

    let ex_ifempty = IFEMPTY (Tete (List [Int 10]), Int 3, Tete (List []))
    let inferred_ifempty = inference ex_ifempty

    let ref = IFZERO (Ref (ref (Int 3)), Int 5, Int 2)
    let inferred_ex_ref = inference ref

    let r = App (Abs ("x", Var "x"), Ref (ref (Int 2)))
    let inferred_r = inference r
    
    let main () =
      print_endline "=================================== Lambda Calculus ===================================";
      print_endline "Identity (I = λx.x) - Beta Reduction :";
      print_endline (print_term reduced_identity);
      print_endline "Inference of Identity (I = λx.x) :";
      print_endline(inferred_identity);
      print_endline "K Function (K = λx.λy.x) - Beta Reduction :";
      print_endline (print_term reduced_k);
      print_endline "Inference of K Function (K = λx.λy.x) :";
      print_endline(inferred_k);
      print_endline "KI Reduction :";
      print_endline (print_term reduced_ki);
      print_endline "Inference of KI :";
      print_endline(inferred_ki);
      print_endline "KII Reduction :";
      print_endline (print_term reduced_kii);
      print_endline "Inference of KII :";
      print_endline(inferred_kii);
      print_endline "S Function (S = λx.λy.λz.xzyz) - Beta Reduction :";
      print_endline "Inference of S Function (S = λx.λy.λz.xzyz) :";
      print_endline(inferred_s);
      print_endline "NAT1 (NAT1 = (λx.(x+1))*3) - Beta Reduction :";
      print_endline "Inference of NAT1 (NAT1 = (λx.(x+1))*3) :";
      print_endline(inferred_nat);
      print_endline "NAT2 (NAT2 = λx.(x+x)) - Beta Reduction :";
      print_endline (print_term reduced_nat1);
      print_endline "Inference of NAT2 (NAT2 = λx.(x+x)) :";
      print_endline(inferred_nat1);
      print_endline "NAT3 (NAT3 = NAT2 * I) - Beta Reduction :";
      print_endline(print_term reduced_nat3);
      print_endline "Inference of NAT3 (NAT3 = NAT2 * I) :";
      print_endline(inferred_nat3);
      print_endline "Omega (ω = (λx.xx)(λy.yy)) - Beta Reduction :";
      print_endline(print_term reduced_omega);
      print_endline "Inference of Omega (ω = (λx.xx)(λy.yy)) :";
      print_endline(inferred_omega);
      print_endline "=================================== PCF ===================================";
      print_endline "Reduction for IFZERO :";
      print_endline(print_term reduced_ifzero);
      print_endline "Inference of IFZERO :";
      print_endline(inferred_ifzero);
      print_endline "Reduction for IFEMPTY :";
      print_endline "Inference of IFEMPTY :";
      print_endline(inferred_ifempty);
      print_endline "=================================== Imperative Traits ===================================";
      print_endline "Inference of EX_REF :";
      print_endline(inferred_ex_ref);
      print_endline "Inference of R :";
      print_endline(inferred_r)
      let _ = main ()
      

