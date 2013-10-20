(* Modules Stdlib *)

let string_of_char c = 
	let s = ref "0" in
	(!s).[0] <- c;
	!s

let string_list_to_array l = 
	let a = Array.make (List.length !l) (ref "") in
	let f range value =
		match !l with [] -> () | e::s ->
		a.(range) <- e;
		l := s
	in Array.iteri f a; a

let array_of_list l =
	let l = ref l in
	Array.init (List.length !l) (fun _ -> match !l with
		| e::s -> l := s ; e
		| _ -> failwith "Error in : Stdlib.array_of_list")

let array_from_refarray a =
	Array.init (Array.length a) (fun i -> !(a.(i)))

let split_string str separator = 
	let l = ref [] and s = ref "" in
	let f = function car ->
		if car = separator then
			(let sc = ref (!s) in
			l := sc::!l;
			s := "")
		else
			(s := (!s)^(string_of_char car)) in
	String.iter f str;
	l := s::!l;
	l := List.rev !l;
	string_list_to_array l

let string_search s f =
	let l = String.length s and
	lf = String.length f and
	ret = ref (-1) and
	i = ref 0 in
	while !i < l do
		if s.[!i] = f.[0] && !i < l - lf && String.sub s !i lf = f then
			(ret := !i;
			i := l)
		else
			i := !i + 1	
	done;
	!ret

let split_string2 str sep =
	let s = ref str and
	l = ref [] in
	while !s <> "" do
		let p = string_search !s sep in
		if p >= 0 then
			(l := (ref (String.sub !s 0 p)) :: !l;
			s := String.sub !s (p + String.length sep) (String.length !s - p - String.length sep);
			if !s = "" then
				l := (ref "") :: !l)
		else
			(l := s :: !l;
			s := "")
	done;
	l := List.rev !l;
	let a = string_list_to_array l in
	array_from_refarray a

let string_of_int4 (a,b,c,d) sep =
	(string_of_int a)^sep^(string_of_int b)^sep^(string_of_int c)^sep^(string_of_int d)

let int4_of_string s sep = 
	let a = split_string s (sep.[0]) in
	if Array.length a <> 4 then (0,0,0,0) else
	(int_of_string !(a.(0)),int_of_string !(a.(1)),int_of_string !(a.(2)),int_of_string !(a.(3)))




let remove_char_from s c =
	let ret = ref "" in
	String.iter (function car ->
		if car <> c then 
			ret := !ret^(string_of_char car)) s;
	!ret

let remove_string_from s c =
	let s = ref s in
	String.iter (function c -> s := remove_char_from !s c) c;
	!s

let rec  stringlist_to_string l separator = match l with
	| e::s::l -> e ^ (string_of_char separator) ^ (stringlist_to_string (s::l) separator)
	| e::_ -> e
	| [] -> ""

let stringarray_to_string arr separator =
	let ret = ref "" in
	Array.iter (function e -> ret := !ret ^ (if !ret = "" then "" else (string_of_char separator)) ^ e) arr;
	!ret

let floatarray_to_string arr separator =
	let ret = ref "" in
	Array.iter (function e ->
		ret := !ret ^ (if !ret = "" then "" else (string_of_char separator)) ^ (string_of_float e)) arr;
	!ret

let doublefloatarray_to_string arr sep1 sep2 = 
	let ret = ref "" in
	Array.iter (function a ->
		ret := !ret ^ (if !ret = "" then "" else (string_of_char sep1)) ^ (floatarray_to_string a sep2)) arr;
	!ret

let string_to_floatarray str separator =
	let str = ref str in
	str := remove_char_from !str '\n';
	str := remove_char_from !str ' ';
	let a = split_string !str separator in
	let ret = Array.make (Array.length a) 0. in
	Array.iteri (fun r v -> 
		try ret.(r) <- float_of_string !v
		with e -> ignore (failwith ("Cannot convert "^(!v)^" to float."))) a;
	ret

let string_to_doublefloatarray str sep1 sep2 =
	let a = split_string str sep1 in
	let ret = Array.init (Array.length a) (function i -> string_to_floatarray !(a.(i)) sep2) in ret
