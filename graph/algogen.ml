open Float
open Tools

let random_path (gr:Graph2.graph) =
  let nodes = get_nodes gr in
    shuffle nodes