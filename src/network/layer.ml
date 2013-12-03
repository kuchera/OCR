let separator = '|'

class layer nb_neuron nb_in =
object (this)
	val mutable _neurons = Array.init nb_neuron (fun _ -> new Neuron.neuron nb_in)

	method get i = _neurons.(i)
	method set i n = _neurons.(i) <- n
	method get_out = Array.init nb_neuron (fun i -> (_neurons.(i))#out)
	method set_allin ain = 
		for i=0 to nb_neuron - 1 do
			this#set_in i ain
		done
	method set_in i ain = _neurons.(i)#set_input ain
	method sum_adj n =
		let out = ref 0. in
		for i=0 to nb_neuron - 1 do
			out := !out +. _neurons.(i)#weight n *. _neurons.(i)#error
		done;
		!out
	method serror i e = _neurons.(i)#serror e
	method adjust_weight rate =
		for i=0 to nb_neuron - 1 do
			_neurons.(i)#adjust_weight rate
		done
	method length = nb_neuron
	method set_neuron i n = _neurons.(i) <- n
	method to_string =
		let nar = Array.init nb_neuron (fun i -> _neurons.(i)#to_string) in
		(string_of_int nb_in) ^ (Stdlib.string_of_char separator) ^ (Stdlib.stringarray_to_string nar separator)
end

let parse str =
	let nar = Stdlib.split_string2 str (Stdlib.string_of_char separator) in
	let n = new layer (Array.length nar - 1) (int_of_string nar.(0)) in
	for i=0 to Array.length nar - 1 do
		n#set_neuron i (Neuron.parse (nar.(i + 1)))
	done;
	n
