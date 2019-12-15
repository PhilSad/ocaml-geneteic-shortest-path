open Graph2

let clone_nodes gr = List.map (fun (a,b) -> (a, []) ) gr

let shuffle l =
  let rand = List.map (fun e -> (Random.bits (), e ) ) l in
  let sorted_rand = List.sort (fun a b -> fst a - fst b)  rand in
    List.map snd sorted_rand;;

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