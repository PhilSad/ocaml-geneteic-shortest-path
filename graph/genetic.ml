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

graph_to_indivs pop;;

div 2.0  5.6;;


let nb_elits = int_of_float (div (mul 49. (float_of_int (List.length pop))) 100. ) 


open Float;;

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

        let rand = float_of_int (Random.int 101) in
        let tot_fitness = List.fold_left (fun acc indiv -> add indiv.fit acc) 0. pop in
        let roul = div (mul rand tot_fitness) 100.  in
        let rec find_gagnant ranked_pop roul sumacc = match ranked_pop with
          | [] -> failwith "impossible de trouver un gagnant"
          | {chem = c ; fit = f} :: rest ->
              print_list ranked_pop;
              if add sumacc f < roul 
              then find_gagnant rest roul (add sumacc f )
              else 
                {chem = c ; fit = f}
        in

          find_gagnant ranked_pop roul 0. :: get_roulette ranked_pop (nb-1)
  in

  let nb_elits = perc_to_nb (List.length pop) perct_elit in
  let nb_roulette = int_of_float (div (float_of_int (List.length pop)) 2. ) - nb_elits in

  let elits = get_elits ranked_pop nb_elits in
  let gagnants = get_roulette ranked_pop nb_roulette in

    List.append elits gagnants;;


let rec print_list = function
  | [] -> Printf.printf "\n%!"
  | {chem = c ; fit = f} :: rest -> Printf.printf "%f |" f ; print_list rest;;

let pop = [{chem = []; fit = 3.};{chem = []; fit = 1.};{chem = []; fit = 2.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.};{chem = []; fit = 1.}];;
print_list  pop;;
selection pop 0.;;



let a,b = 1,2;;
let a,b = b,a;;

(* CROSSOVER *)


let shuffle l =
  let rand = List.map (fun e -> (Random.bits (), e ) ) l in
  let sorted_rand = List.sort (fun a b -> fst a - fst b)  rand in
    List.map snd sorted_rand;;

let a = ['a';'b';'c'];;
shuffle a;;


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

let rec cross_over pop =  
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

          let enfant1 = {chem = new_chem1; fit = fitness new_chem1} in
          let enfant2 = {chem = new_chem2; fit = fitness new_chem2} in

            enfant1 :: enfant2 :: cross_over rest;;
;;




let pop = [{chem = ([1;2;3;4;5;6;7;8]; fit = 10};{chem = [10;20;30;40;50;60;70;80]; fit = 10}]

let p1 = List.hd pop;;

get_slice p1.chem 0 cut1


let enfant1 = List.append (get_slice p1 0




































