open Algogen;;
open Gfile2;;

let graph = from_file "graph2";;

genetic_algo graph 10 10. 5 10;;
