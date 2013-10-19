(* Entry point of the program *)
(* using : Iostream *)

let print s = Printf.printf "%s\n" s

let _ =
	let s = "42;4;2;42" in
	print (Stdlib.string_of_int4 (Stdlib.int4_of_string s ";") "|")
