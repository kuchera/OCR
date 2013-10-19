(* Module Network *)

let print s = Printf.printf "%s\n" s

let _ = 
	let l = new Layer.layer 3 5 in
	print (l#to_string)
