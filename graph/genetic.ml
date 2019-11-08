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

Float.sub 2.5 1.5;;

sqrt 9.**2;;

(* Fitness function = distance entre les noeuds de la liste*)

let rec fitness (gr:Graph2.graph) = match gr with
  | [] -> 0.
  | [n] -> 0. 
  | (_, (x1, y1)) :: rest -> 
      let _, (x2, y2) = List.hd rest in
      let dist_to_next = sqrt ( (Float.add ((Float.sub x2 x1)**2.)  ( (Float.sub y2  y1)**2. ) )  ) in
        Printf.printf "%f %f %f %f\n" x1 y1 x2 y2;
        Float.add dist_to_next (fitness rest)

let p = random_path graph;;

fitness p;;
graph;;


type individu = {chem : Graph2.graph; fit : float};;

let graph_to_indiv gr = {chem = gr; fit = div 1. (fitness gr)};; 
let rec graphs_to_indivs pop = List.map graph_to_indiv pop;;

let pop = n_random_path graph 100;;

graphs_to_indivs pop;;

graph_to_indiv graph;;

div 2.0  5.6;;


let nb_elits = int_of_float (div (mul 49. (float_of_int (List.length pop))) 100. ) 


open Float;;

(* SELECTION : roullette wheel + elitisme *)


(* return int(perc% of n) *) 
let perc_to_nb n perc = int_of_float (div (mul perc (float_of_int n)) 100. ) 

let selection pop perct_elit=
  let sort_fun = fun p1 p2 -> if sub p1.fit p2.fit > 0. then -1 else 1 in
  let ranked_pop = List.sort sort_fun pop in

  let rec get_elits ranked_pop nb = match (nb, ranked_pop) with
    | 0,_  -> []
    | _,[] -> []
    | nb, head :: rest -> head :: get_elits rest (nb-1)
  in

  (* pourcentage à garder -> nb à garder *)
  let nb_elits = perc_to_nb (List.length pop) perct_elit in

    Printf.printf "%d\n" nb_elits;

    let nb_roulette = List.length pop - nb_elits in

    let rec get_roulette ranked_pop nb = match (nb, ranked_pop) with
      | _, [] -> []
      | 0, _  -> []
      | nb, ranked_pop ->

          let rand = float_of_int (Random.int 101) in
          let tot_fitness = List.fold_left (fun acc indiv -> add indiv.fit acc) 0. pop in
            Printf.printf "tot_fit = %f\n" tot_fitness;
            let roul = div (mul rand tot_fitness) 100.  in
              Printf.printf "%f\n" roul;
              let rec find_gagnant ranked_pop roul sumacc = match ranked_pop with
                | [] -> failwith "impossible de trouver un gagnant"
                | {chem = c ; fit = f} :: rest ->
                    Printf.printf "fit = %f sumacc = %f\n" f (add sumacc f);
                    print_list ranked_pop;
                    if add sumacc f < roul 
                    then find_gagnant rest roul (add sumacc f )
                    else 
                      {chem = c ; fit = f}
              in

                find_gagnant ranked_pop roul 0. :: get_roulette ranked_pop (nb-1)
    in

    let elits = get_elits ranked_pop nb_elits in
    let gagnants = get_roulette ranked_pop nb_roulette in

      List.append elits gagnants;;


let rec print_list = function
  | [] -> Printf.printf "\n%!"
  | {chem = c ; fit = f} :: rest -> Printf.printf "%f |" f ; print_list rest;;

print_list  pop;;

let pop = [{chem = []; fit = 3.};{chem = []; fit = 1.}];;

List.fold_left (fun acc indiv -> add indiv.fit acc) 0. pop;;

pop;;

selection pop 50.;;


let pop = graphs_to_indivs (n_random_path graph 10);;

selection pop 25.;;

perc_to_nb 100 25.;;


let tot_fitness = List.fold_left (fun acc indiv -> 0 indiv.fit + acc) 0 pop

let elits = get_elits ranked_pop 2 in
  elits;;

1.5+ 2.5;;


let tot_fitness = List.fold_left (fun acc indiv -> add indiv.fit acc) 0. pop;;

2.0 > 1.0;;

Random.int 101;;

int_of_float 2.9





























