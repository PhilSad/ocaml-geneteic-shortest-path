open Graph2

let clone_nodes gr = List.map (fun (a,b) -> (a, []) ) gr

let shuffle l =
  let rand = List.map (fun e -> (Random.bits (), e ) ) l in
  let sorted_rand = List.sort (fun a b -> fst a - fst b)  rand in
    List.map snd sorted_rand;;