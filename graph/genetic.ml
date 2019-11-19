
open Float;;
open Graph2;;
let graph = Gfile2.from_file "graph2";;

type path = Graph2.id list;;

let i,pos = List.nth graph 2;;

Random.int (List.length graph);;

(* Generation de la population initiale *)


let shuffle l =
  let rand = List.map (fun e -> (Random.bits (), e ) ) l in
  let sorted_rand = List.sort (fun a b -> fst a - fst b)  rand in
    List.map snd sorted_rand;;

let a = ['a';'b';'c'];;
shuffle a;;


let random_path (gr:Graph2.graph) =
  let nodes = get_nodes gr in
    shuffle nodes;;

random_path graph;;

let rec n_random_path gr n = match n with
  | 0 -> []
  | n -> random_path gr :: n_random_path gr (n-1) 
;;
n_random_path graph 5;;


(* Fitness function = distance entre les noeuds de la liste*)

let rec fitness gr path = div 1.(Graph2.distance_tot gr path);;

fitness graph (random_path graph);;

let p = random_path graph;;

fitness p;;
graph;;

distance_tot graph p;;

type individu = {chem : path; fit : float};;


let path_to_indiv gr path  = {chem = path; fit =  (fitness gr path)};; 
let rec paths_to_indivs gr pop = List.map (path_to_indiv gr) pop  ;;

let pop = n_random_path graph 100;;

paths_to_indivs graph pop;;


(* SELECTION : roullette wheel + elitisme *)


(* return int(perc% of n) *) 
let perc_to_nb n perc = int_of_float (div (mul perc (float_of_int n)) 100. ) 

let selection pop perct_elit=
  let sort_fun = fun p1 p2 -> if sub p1.fit p2.fit > 0. then -1 else 1 in
  let ranked_pop = List.sort sort_fun pop in

  (* Selection elitiste : on garde les perct_elit% meilleur *)
  let rec get_elits ranked_pop nb = match (nb, ranked_pop) with
    | 0,_  -> []
    | _,[] -> []
    | nb, head :: rest -> head :: get_elits rest (nb-1)
  in

  (* Selection par roulette *)
  let rec get_roulette ranked_pop nb = match nb with
    | 0 -> []
    | nb ->

        let rand = float_of_int (Random.int 100) in
        let tot_fitness = List.fold_left (fun acc indiv -> add indiv.fit acc) 0. pop in
        let roul = div (mul rand tot_fitness) 100.  in
        let rec find_gagnant ranked_pop roul sumacc = match ranked_pop with
          | [] -> failwith "impossible de trouver un gagnant "
          | {chem = c ; fit = f} :: rest ->

              if add sumacc f < roul 
              then find_gagnant rest roul (add sumacc f )
              else 
                {chem = c ; fit = f}
        in

          find_gagnant ranked_pop roul 0. :: get_roulette ranked_pop (nb-1)
  in

  let nb_elits = perc_to_nb (List.length pop) perct_elit in
  let nb_roulette = List.length pop - nb_elits in

  let elits = get_elits ranked_pop nb_elits in
  let gagnants = get_roulette ranked_pop nb_roulette in

    List.append elits gagnants;;


let rec print_list = function
  | [] -> Printf.printf "\n%!"
  | {chem = c ; fit = f} :: rest -> Printf.printf "%f |" f ; print_list rest;;

let pop = [{chem = []; fit = 3.};{chem = []; fit = 10.};{chem = []; fit = 2.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.}];;
print_list  pop;;
selection pop 0.;;



let a,b = 1,2;;
let a,b = b,a;;

(* CROSSOVER *)




let get_slice l i_from i_to = 
  let rec take l n = match l with  
    | [] -> []
    | e :: rest -> if n = 0 then [] else e :: take rest (n-1) 
  in

  let rec drop l n = match l with
    | [] -> []
    | e :: rest -> if n = 0 then e :: rest else drop rest (n-1) 
  in
    take (drop l i_from ) (i_to - i_from) ;;


get_slice a 0 2;;

;;


let sanitize_child gr child =
  (* - Parser le chemin et mettre -1 si un item apparait plus d'une fois
     - Récupérer les id qui ne sont pas dans le child puis shuffle
     - pour chaque -1 de la liste mettre le premier de la liste ^*)
  let rec signal_duplicates path = match path with
    | [] -> []
    | x :: rest ->
        (if List.exists (fun o -> x == o) rest then -1
         else x)
        :: signal_duplicates rest
  in

  let rec not_in l1 l2 = match l1 with
    | [] -> []
    | x :: rest ->

        let pred = fun o -> not (List.exists (fun a-> o == a) l2) in
          List.filter pred l1
  in

  let dups = signal_duplicates child in

  let others = shuffle (not_in (get_nodes gr) dups) in

  let rec replace_dup dup others = match (dup, others) with
    | [], _ -> []
    | -1 :: rest, hd :: tl -> hd :: replace_dup rest tl
    | x :: rest, _ -> x :: replace_dup rest others
  in 

    replace_dup dups others
;;

sanitize_child graph [1;1;2;2;3];;

get_nodes graph;;

let rec cross_over gr pop =  
  let rand_pop = shuffle pop in
    match rand_pop with
      | []  -> []
      | [x] -> [x]
      | p1 :: p2 :: rest ->

          let pick_cuts () = 
            let r1 = Random.int (List.length p1.chem) in
            let r2 = Random.int (List.length p1.chem) in
              if r1 > r2 then r2,r1 else r1,r2
          in

          let cut1, cut2 = pick_cuts () in
          let new_chem1 = (get_slice p1.chem 0 cut1) @ (get_slice p2.chem cut1 cut2) @ (get_slice p1.chem cut2 (List.length p1.chem) ) in
          let new_chem2 = (get_slice p2.chem 0 cut1) @ (get_slice p1.chem cut1 cut2) @ (get_slice p2.chem cut2 (List.length p1.chem) ) in

          let san_chem1 = sanitize_child gr new_chem1 in
          let san_chem2 = sanitize_child gr new_chem2 in


          let enfant1 = {chem =  san_chem1; fit = fitness gr san_chem1} in
          let enfant2 = {chem =  san_chem2; fit = fitness gr san_chem2} in

            enfant1 :: enfant2 :: cross_over gr rest;;
;;





(* MUTATION swap *)


let swap l a b  = 
  let va = List.nth l a in
  let vb = List.nth l b in
  let rec swp l a b i = match l with
    | [] -> []
    | x :: rest ->
        if i == a then vb :: swp rest a b (i+1)
        else if  i == b then va:: swp rest a b (i+1) 
        else x :: swp rest a b (i+1)
  in
    swp l a b 0
;;
let a = [10;20;30;40;50;60;70];;
swap a 2 2;; 




(* A tester *)
let rec mutate_pop gr pop tx = match pop with
  | [] -> []
  | p :: rest ->
      let gamma = Random.int 101 in
      let len = List.length p.chem in
        if gamma < tx then 
          let chem = swap p.chem (Random.int len) (Random.int len) in

            {chem = chem; fit = fitness gr chem} :: mutate_pop gr rest tx

        else
          p :: mutate_pop gr rest tx;;


graph;;











let mean_fitness pop =
  let rec sum l = match l with 
    | [] -> 0.
    | p :: rest -> add p.fit (sum rest)
  in
    div (sum pop) (float_of_int (List.length pop));;





let genetic_algo nb_pop tx_elitisme tx_iradiation nb_generation =
  let population = paths_to_indivs graph (n_random_path graph nb_pop) in

  let rec loop population gen_rest = match gen_rest with
    | 0 -> population
    | n ->
        let survivants = selection population tx_elitisme in
        let next_pop   = cross_over graph survivants in
        let mutations  = mutate_pop graph next_pop tx_iradiation in
          Printf.printf "Generation %d fitness moyen : %f nb_pop : %d\n" gen_rest (mean_fitness mutations) (List.length survivants);
          loop mutations (gen_rest - 1) 
  in
    loop population nb_generation;;


genetic_algo 200 10. 5 1000;;

















let pop = [{chem = ([1;2;3;4;5;6;7;8]; fit = 10};{chem = [10;20;30;40;50;60;70;80]; fit = 10}]

let p1 = List.hd pop;;

get_slice p1.chem 0 cut1


let enfant1 = List.append (get_slice p1 0




































