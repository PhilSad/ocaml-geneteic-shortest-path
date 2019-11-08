type id = int;;

type pos = float * float;;

type graph = (id * pos) list;;

exception Graph_error of string



let empty_graph = [];;

let node_exists gr id = List.mem_assoc id gr

let add_node (gr:graph) (id:int) x y = 
    if node_exists gr id then raise (Graph_error ("Node " ^ string_of_int id ^ " already exists in the graph."))
    else
        (id, (x,y)) :: gr

let node_pos gr id =   
    try List.assoc id gr
    with Not_found -> raise (Graph_error ("Node " ^ string_of_int id ^ " does not exist in this graph."))


let rec get_nodes gr = match gr with
    | [] -> []
    | x :: rest -> fst x :: get_nodes rest

let rec get_node_pos gr id = match gr with
    | [] -> failwith "Error get_node_pos : node non trouvé"
    | x :: rest -> if fst x = id then snd else get_node_pos rest id