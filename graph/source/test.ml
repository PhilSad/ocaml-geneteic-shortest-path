open Algogen;;
open Gfile2;;

let graph = from_file "../graphes/graph1";;

genetic_algo graph 10 10. 5 10;;
