(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph2

type path = string

(* Values are read as strings. *)
val from_file: path -> graph


(* The format of files is compatible with the files generated by:
   https://www-m9.ma.tum.de/graph-algorithms/flow-ford-fulkerson/index_en.html
*)
