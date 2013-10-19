(* Entry point of the program *)
(* using : Iostream *)

let print s = Printf.printf "/%s/\n" s

let _ =
Array.iter print (Stdlib.split_string2 "llallbllllcllll" "ll");
print "========";
Array.iter print (Stdlib.split_string2 "allbllllc" "ll")
