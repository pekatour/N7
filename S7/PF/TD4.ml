(*Exercice 1*)
module type Collection =
sig
  type 'e t
  val est_vide : 'e t -> bool
  val cree : 'e t
  val ajoute : 'e -> 'e t -> 'e t
  val retire : 'e t -> 'e * 'e t
  exception CollectionVide
end

module Pile : Collection =
struct
  type 'e t = 'e list

  exception CollectionVide

  let est_vide l = l = []

  let cree = []

  let ajoute e l = e::l

  let retire l = match l with
    | [] -> raise CollectionVide
    | t::q -> (t,q)
end

module File : Collection =
struct
  type 'e t = {
    avant : 'e list;
    arrière : 'e list;
  }

  exception CollectionVide

  let est_vide file =
    (file.avant = [] && file.arrière = [])

  let cree =
    { avant = []; arrière = [] }

  let ajoute element file =
    { avant = file.avant; arrière = element :: file.arrière }

  let retire file =
    match file.avant with
    | x :: xs -> (x, { avant = xs; arrière = file.arrière })
    | [] ->
        match List.rev file.arrière with
        | [] -> raise CollectionVide
        | x :: xs -> (x, { avant = xs; arrière = [] })

end

(*Exercice 2*)

module type Folder =
sig
  type a
  type b
  val cas_de_base : b
  val traiter_cas_general : a -> b -> b
end

module CreerListe : Folder with type a = int and type b = int list =
struct
  type a = int
  type b = int list
  let cas_de_base = []
  let traiter_cas_general e n = e::n 
end

module TrouverPair : Folder with type a = int and type b = int option =
struct
  type a = int
  type b = int option
  let cas_de_base = None
  let traiter_cas_general e pq =
    if (e mod 2 = 0) then Some e
    else pq
end

(*Exercice 3*)
module FoldList (F : Folder) =
struct
  let rec fold_right l =
    match l with
    | [] -> F.cas_de_base
    | t::q -> F.traiter_cas_general t (fold_right q)
end

module FoldCol (C: Collection) (F: Folder) =
struct
  let rec fold c =
    if C.est_vide c then
      F.cas_de_base
    else
      let (e,r) = C.retire c in
        F.traiter_cas_general e (fold r)
end

type 'a arbre =
  |Empty
  |Node of 'a * 'a arbre * 'a arbre

module FoldArbre (C: Collection) (F: Folder) =
struct
  let fold arbre =
    let rec parcours c =
      if C.est_vide c then
        F.cas_de_base
      else
        let (e,r) = C.retire c in
          match e with
            | Empty -> parcours r
            | Node(n,g,d) -> F.traiter_cas_general n (parcours (C.ajoute d (C.ajoute g r)))
  in parcours (C.ajoute arbre C.cree)
end

(*q1*) module CreerListeProfondeur = FoldArbre (Pile) (CreerListe)
(*q2*) module CreerListeProfondeur = FoldArbre (File) (CreerListe)
(*q3*) module CreerListeProfondeur = FoldArbre (Pile) (TrouverPair)
(*q4*) module CreerListeProfondeur = FoldArbre (File) (CreerListe)


