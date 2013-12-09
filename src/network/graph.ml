(* [|42;42;42|] est un array *)
let separator = '#'

class graph learn_rate nperl =
object (this)
	val mutable _layers = Array.init (Array.length nperl - 2) (fun i -> new Layer.layer nperl.(i) (if i=0 then  nperl.(0) else nperl.(i-1)))
	val mutable _output = new Layer.layer nperl.(Array.length nperl - 1) nperl.(Array.length nperl - 2)

	method set_layers a =
		_output <- a.(Array.length a - 1);
		_layers <- Array.init (Array.length a - 1) (fun i -> a.(i))
	method get_layer i =
		if i >= Array.length nperl - 1 then
			_output
		else
			_layers.(i)
	method training ain aout = (* entrees resultatsAttendus *)
		let out = this#get_out ain in
		for i=0 to _output#length - 1 do
			_output#serror i (out.(i) *. (1. -. out.(i)) *. (aout.(i) -. out.(i)))
		done;
		let out = _layers.(Array.length _layers - 1)#get_out in
		for i=0 to _layers.(Array.length _layers - 1)#length - 1 do
			_layers.(Array.length _layers - 1)#serror i (out.(i) *. (1. -. out.(i)) *. (_output#sum_adj i))
		done;
		let i = ref (Array.length _layers - 2) in
		while !i >= 0 do
			let out = _layers.(!i)#get_out in
			for j=0 to _layers.(!i)#length - 1 do
				_layers.(!i)#serror j (out.(j) *. (1. -. out.(j)) *. (_layers.(!i + 1)#sum_adj j))
			done;
			i := !i - 1
		done;
		_output#adjust_weight learn_rate;
		for i=0 to Array.length _layers - 1 do
			_layers.(i)#adjust_weight learn_rate
		done
	method get_out ain = 
		_layers.(0)#set_allin ain;
		for i=1 to Array.length _layers - 1 do
			_layers.(i)#set_allin _layers.(i-1)#get_out
		done;
		_output#set_allin _layers.(Array.length _layers - 1)#get_out;
		_output#get_out
	method get_max_out ain =
		let m = ref 0. and r = ref 0 in
		Array.iteri (fun i e -> if e > !m then (m := e; r := i)) (this#get_out ain);
		(!r, !m)
	method to_string =
		let a = Array.init (Array.length _layers) (fun i -> _layers.(i)#to_string) in
		(string_of_float learn_rate) ^ (Stdlib.string_of_char separator) ^ (Stdlib.stringarray_to_string a separator) ^ (Stdlib.string_of_char separator) ^ (_output#to_string)
	method nb_in = nperl.(0)
end

let parse str =
	let a = Stdlib.split_string2 str (Stdlib.string_of_char separator) in
	let lr = float_of_string (a.(0)) in
	let a = Array.init (Array.length a - 1) (fun i -> Layer.parse (a.(i + 1))) in
	let rec f i l =
		if i >= 0 then 
			f (i-1) ((a.(i)#length) :: l)
		else
			l in
	let g = new graph lr (Stdlib.array_of_list (f (Array.length a - 1) [])) in
	g#set_layers a;
	g
