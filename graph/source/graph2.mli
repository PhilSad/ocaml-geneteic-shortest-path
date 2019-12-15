type id = int
type pos = float * float
type graph

exception Graph_error of string

(* node_exists gr id  indicates if the node with identifier id exists in graph gr. *)
val node_exists: graph -> id -> bool

val add_node:  graph -> int -> float -> float -> graph

val empty_graph: graph

(* Find the out_arcs of a node.
 * @raise Graph_error if the id is unknown in the graph. *)
val node_pos:  graph -> id -> pos

val get_nodes: graph -> id list

val distance_tot: graph -> int list -> float

val get_nb_nodes: graph -> int
