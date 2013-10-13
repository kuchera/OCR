(* Module Serialization *)
(* using : Stdlib Iostream *)

let separator1 = ':'
let separator2 = ';'
let separator3 = '/'

let write_floatarray arr filename = 
	let file = new Iostream.file filename in
	file#add_string ((Stdlib.floatarray_to_string arr separator1)^"\n")

let read_floatarray filename =
	let file = new Iostream.file filename in
	Stdlib.string_to_floatarray (file#read_string) separator1

let write_doublefloatarray arr filename =
	let file = new Iostream.file filename in
	file#add_string ((Stdlib.doublefloatarray_to_string arr separator2 separator1)^"\n")

let read_doublefloatarray filename =
	let file = new Iostream.file filename in
	Stdlib.string_to_doublefloatarray (file#read_string) separator2 separator1
