(* Modules Stdlib *)

let string_list_to_array l = (* l : string ref list ref *)
	let a = Array.make (List.length !l) (ref "") in
	let f range value =
		match !l with [] -> () | e::s ->
		a.(range) <- e;
		l := s
	in Array.iteri f a; a
