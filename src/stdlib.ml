(* Modules Stdlib *)

let string_list_to_array l = (* l : string ref list ref *)
	let a = Array.make (List.length !l) (ref "") in
	let f range value =
		match !l with [] -> () | e::s ->
		a.(range) <- e;
		l := s
	in Array.iteri f a; a

let split_string str separator = 
	let l = ref [] and s = ref "" in
	let f = function car ->
		if car = separator then
			(let sc = ref (!s) in
			l := sc::!l;
			s := "")
		else
			(s := (!s)^(Char.escaped car)) in
	String.iter f str;
	l := s::!l;
	l := List.rev !l;
	string_list_to_array l

let remove_char_from s c =
	let ret = ref "" in
	String.iter (function car ->
		if car <> c then 
			ret := !ret^(Char.escaped car)) s;
	!ret

let floatarray_to_string arr separator =
	let ret = ref "" in
	Array.iter (function e ->
		ret := !ret ^ (if !ret = "" then "" else (Char.escaped separator)) ^ (string_of_float e)) arr;
	!ret

let doublefloatarray_to_string arr sep1 sep2 = 
	let ret = ref "" in
	Array.iter (function a ->
		ret := !ret ^ (if !ret = "" then "" else (Char.escaped sep1)) ^ (floatarray_to_string a sep2)) arr;
	!ret

let string_to_floatarray str separator =
	let str = ref str in
	str := remove_char_from !str ' ';
	let a = split_string !str separator in
	let ret = Array.make (Array.length a) 0. in
	Array.iteri (fun r v -> ret.(r) <- float_of_string !v) a;
	ret

let string_to_doublefloatarray str sep1 sep2 =
	let a = split_string str sep1 in
	let ret = Array.init (Array.length a) (function i -> string_to_floatarray !(a.(i)) sep2) in ret
