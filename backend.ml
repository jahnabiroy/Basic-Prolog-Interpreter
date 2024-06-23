exception NOT_UNIFIABLE
exception NOT_DEFINED

type variable = string
type predicate = string
type term = 
  V of variable 
  | CInt of int 
  | ATOM of predicate * (term list)
type atom = A of predicate * (term list)
type head = H of atom
type body = B of atom list
type clause = 
  Fact of head 
  | Rule of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list

let rec occurs x y = match y with
    [] -> false
  | z::ys -> (x = z) || (occurs x ys)
;;

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::xs -> if (occurs x l2) then union xs l2
             else x::(union xs l2)
;;

let rec unique_terms (i:int) (t:term): term = match t with
    V(v) -> V((string_of_int i) ^ v)
  | ATOM(s, l) -> ATOM(s, List.map (unique_terms i) l)
  | _ -> t
;;

let rec unique_atoms (i:int) (a:atom): atom = match a with
  A(s, l) -> A(s, List.map (unique_terms i) l)
;;

let rec unique_clause (cl:clause) (i:int): clause = match cl with
    Fact(H(a)) -> Fact(H(unique_atoms i a))
  | Rule(H(a), B(l)) -> Rule(H(unique_atoms i a), B(List.map (unique_atoms i) l))
;;

let rec unique_prog (prog:program) (i:int): program = match prog with
    [] -> []
  | cl::ps -> (unique_clause cl i)::unique_prog ps (i+1)
;;

let rec unique (prog:program) (A(s, _): atom): program = match prog with
    [] -> []
  | cl::ps -> match cl with Fact(H(A(s', _))) | Rule(H(A(s', _)), _) ->
                if s = s' then (unique_clause cl 0)::unique ps (A(s, []))
                else cl::unique ps (A(s, []))
;;

let rec vars_term (t:term): variable list =
  match t with
      V(v) -> [v]
    | ATOM(s, l) -> List.fold_left union [] (List.map vars_term l)
    | _ -> []
;;

let vars_atom (A(s, l): atom): variable list = vars_term (ATOM(s, l))
;;

let rec vars_goal (G(g): goal): variable list = List.fold_left union [] (List.map vars_atom g)
;;

let rec subst (s:substitution) (t:term): term =
  match t with
      ATOM(s', l) -> ATOM(s', List.map (subst s) l)
    | CInt(_) -> t
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

let rec subst_atom (s:substitution) (A(s', l): atom): atom = A(s', List.map (subst s) l)
;;

let rec exists_vars_in_term (v:variable) (t:term): bool =
  match t with
      V(x) -> x = v
    | ATOM(s, l) -> List.fold_left (||) false (List.map (exists_vars_in_term v) l)
    | _ -> false
;;

let compose (s1:substitution) (s2:substitution): substitution =
  let f s x = (fst x, subst s (snd x)) in (List.map (f s2) s1) @ s2
;;

let rec mgu_term (t1:term) (t2:term): substitution =
  match (t1, t2) with
      (V(x), V(y)) -> if x = y then []
                      else [(x, V(y))]
    | (V(x), ATOM(_, _)) -> if exists_vars_in_term x t2 then raise NOT_UNIFIABLE
                            else [(x, t2)]
    | (ATOM(_, _), V(y)) -> if exists_vars_in_term y t1 then raise NOT_UNIFIABLE
                            else [(y, t1)]
    | (CInt(n1), CInt(n2)) -> if n1 = n2 then [] else raise NOT_UNIFIABLE
    | (CInt(n1), V(x)) -> [(x, t1)]
    | (V(x), CInt(n2)) -> [(x, t2)] 
    | (ATOM(s1, l1), ATOM(s2, l2)) ->
        if s1 <> s2 || (List.length l1 <> List.length l2) then raise NOT_UNIFIABLE
        else
          let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
          List.fold_left f [] (List.combine l1 l2)
    | _ -> raise NOT_UNIFIABLE
;;

let mgu_atom (A(s1, l1): atom) (A(s2, l2): atom): substitution = mgu_term (ATOM(s1, l1)) (ATOM(s2, l2))
;;

let rec print_term_list (tl:term list) = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ",";
      print_term_list tls;
    )

and print_list_body (t:term) = match t with
    ATOM("empty_list", []) -> Printf.printf ""
  | ATOM("list", [t1; ATOM("empty_list", [])]) -> print_term t1
  | ATOM("list", [t1; t2]) -> (
      print_term t1;
      Printf.printf ",";
      print_list_body t2;
    )
  | _ -> raise NOT_DEFINED

and print_term (t:term) = match t with
    V(v) -> Printf.printf " %s " v
  | ATOM("empty_list", []) -> Printf.printf " [] "
  | ATOM(s, []) -> Printf.printf " %s " s
  | ATOM("list", _) -> (
      Printf.printf " [";
      print_list_body t;
      Printf.printf "] ";
    )
  | ATOM(s, l) -> (
      Printf.printf " %s ( " s;
      print_term_list l;
      Printf.printf " ) ";
    )
  | CInt(n) -> Printf.printf " %d " n
;;

let rec getSolution (unif:substitution) (vars:variable list) = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NOT_DEFINED
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getSolution unif vs
      with NOT_DEFINED -> getSolution unif vs
;;
  
let rec printSolution (unif:substitution) = match unif with
    [] -> Printf.printf ""
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printSolution xs;
    )
;;

let solve_atom_atom (a1:atom) (a2:atom) (unif:substitution): substitution =
  compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))
;;

let solve_term_term (t1:term) (t2:term) (unif:substitution): substitution =
  compose unif (mgu_term (subst unif t1) (subst unif t2))
;;

let rec solve_goal (prog:program) (g:goal) (unif:substitution) (vars:variable list): (bool * substitution) =
  match g with
      G([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        (true, [])
      )
    | G(a::gs) -> match a with
        | A("cut", _) -> let _ = solve_goal prog (G(gs)) unif vars in (true, [])
        | _ ->
          let new_prog = unique prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                Fact(H(a')) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
              | Rule(H(a'), B(al)) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(al @ gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
        in iter prog
;;

let interpret_goal (prog:program) (g:goal) = solve_goal prog g [] (vars_goal g)
;;
