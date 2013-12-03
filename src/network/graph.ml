(* [|42;42;42|] est un array *)
let separator = '#'

class graph nb_in learn_rate nperl =
object (this)
	val mutable _layers = Array.init (Array.length nperl - 2) (fun i -> new Layer.layer nperl.(i) (if i=0 then nb_in else nperl.(i-1)))
	val mutable _output = new Layer.layer nperl.(Array.length nperl - 1) nperl.(Array.length nperl - 2)

	method to_string =
		let a = Array.init (Array.length nperl) (fun i -> if i = Array.length nperl - 1 then _output#to_string else _layers.(i)#to_string) in
		Stdlib.stringarray_to_string a separator
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
end
