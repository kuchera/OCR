(* Entry point of the program *)
(* using : Iostream *)

let _ = 
let s = "1.1;1.2;1.3/2.1;2.2;2.8/3.654;3.9/321.2654;0.;6542.654" in
Array.iter (function arr -> Array.iter (Printf.printf "%f ") arr; Printf.printf "\n") (Stdlib.string_to_doublefloatarray s '/' ';');
Printf.printf "\n"
