(*Exercice 1*)
type 'a arbre = Noeud of (('a arbre * 'a) list) * bool ;;

(*Exercice 2*)
let rec appartient arbre a = match arbre, a with
  | Noeud(l,b),[] -> b
  | Noeud(l,b),c1::q1 -> match l with
    | [] -> false
    | (arb,c2)::q2-> 
      if c1=c2 then 
        appartient arb q1
      else if c1 < c2 then 
        false
      else 
        appartient (Noeud(q2,b)) a;;

(*Exercice 3*)
let rec ajout arbre a = match arbre, a with
  | Noeud(l,b),[] -> Noeud(l,true)
  | Noeud(l,b),c1::q1 -> 

    let rec ajoute_liste l = match l with
    | [] -> [(ajout (Noeud([],false)) q1,c1)]
    | (arb,c2)::q2-> 
      if c1=c2 then
        (ajout arb q1,c2)::q2
      else if c1 < c2 then 
        (ajout (Noeud([],false)) q1,c1)::(arb,c2)::q2
      else (arb,c2)::ajoute_liste q2
    in

  Noeud(ajoute_liste l, b);;

let abr_test = Noeud([],false) in
let a1 = ajout abr_test ['a';'b';'c'] in
let a2 = ajout a1 ['a';'b';'d'] in appartient a2 ['a';'b'];;

(*Exercice 4*)
type ('a,'b) trie = 'a arbre * ('b -> 'a list) * ('a list -> 'b);;

(*Exercice 5*)
let appartient_trie (arb,fd,fr) a =
  appartient (fd a) arb;;

(*Exercice 6*)
let ajout_trie (arb,fd,fr) a =
  (ajout (fd a) arb,fd,fr);;

let rec print_arb arbre = match arbre with (*low effort*)
  | Noeud([],b) -> ();
  | Noeud(l,b) -> let _ = List.map (fun (abr,c) -> print_char c; print_arb abr; print_newline()) l in ();;