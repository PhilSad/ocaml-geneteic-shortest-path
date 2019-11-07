let graph = Gfile2.from_file "graph1";;

let i,pos = List.nth graph 2;;

Random.int (List.length graph);;




(* Generation de la population initiale *)

let rec random_path (gr:Graph2.graph) = match gr with
  | [] -> []
  | gr ->
      (* On pioche un élément random de la liste qu'on met au début du chemin généré *)
      let i_rem = Random.int (List.length gr) in
      let id,pos = List.nth gr i_rem in
      let graph_rem = List.remove_assoc id gr in
        (id,pos) :: random_path graph_rem
;;
random_path graph;;

let rec n_random_path gr n = match n with
  | 0 -> []
  | n -> random_path gr :: n_random_path gr (n-1) 
;;
n_random_path graph 5;;

2.**3.;;

sqrt 9.**2;;

(* Fitness function *)

let rec fitness gr = match gr with
  | [] -> 0
  | _, (x1, y1) :: rest -> 
      let _, x2, y2 = List.hd gr in
      let dist_to_next = sqrt (x2 - x1)


