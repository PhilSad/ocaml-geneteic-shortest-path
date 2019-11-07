open Graph

let clone_nodes gr = List.map (fun (a,b) -> (a, []) ) gr

let rec gmap gr f = match gr with
    | [] -> []
    | noeud, outs :: rest -> (noeud, List.map f outs) :: gmap rest f

