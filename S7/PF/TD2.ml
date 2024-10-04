(*Exercice 1
Parties de ensemble vide : 1, c'est ensemble contenant ensmeble vide
Récurrence : compter nb de parties d'un ensemble de n+1 éléments à partir des parties d'un ensemble de n éléments
Pour ça pour chaque E € P(n), on a 2 choix : mettre l'élément ou pas.
2^n
*)

(*Exercice 2*)

let ajout e ensemble =
  List.fold_right (fun x l -> (e::x)::x::l) ensemble [];;

let%test _ = ajout 3 [[1;2];[9;2]] = [[3; 1; 2]; [1; 2]; [3; 9; 2]; [9; 2]]
let%test _ = ajout 1 [] = []

let parties ensemble = 
  List.fold_right ajout ensemble [[]];;

(*Exercice 3
 Taille 0 : 1 permutation
Récurrence : n+1 choix pour chaque permutation d'emplacement où placer le N+1 ième élément : n+1 * n!
*)

(*Exercice 4*)
let rec insertions e l =
  match l with
  | [] -> [[e]]
  | t::q -> (e::l)::(List.map (fun l -> t::l) (insertions e q));;

let%test _ = insertions 2 [3;4;5] = [[2; 3; 4; 5]; [3; 2; 4; 5]; [3; 4; 2; 5]; [3; 4; 5; 2]];;


let insertions2 e l = (*Baptiste lol*)
  let rec aux l1 l2 res = match l2 with
    | [] -> res
    |t::q -> aux (t::l1) q ((List.rev_append l1 (e::l2))::res)
  in aux [] l [l@[e]];;

let rec permutations l = match l with
  | [] -> [[]]
  | t::q -> List.flatten (List.map (insertions t) (permutations q));;

let permutations2 l =
  List.fold_right (fun x res_perm -> List.flatten (List.map (insertions x) res_perm)) l [[]];;

(*Exercice 5
k parmi n = (k-1) parmi (n-1) + k parmi (n-1)
*)
(* combinaisons : 'a list -> int -> 'a list list
argument : l, liste
           k, nombre d'éléments à tirer dans l
résultat : liste de combinaisons de k éléments de l
*)