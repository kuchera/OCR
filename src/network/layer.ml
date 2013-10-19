(* Module Layer *)

let separator = '|'

class layer nb_neur nb_out =
    object (this)
        val neurones = Array.init nb_neur (fun i -> (new Neuron.neuron (string_of_int i) nb_out))

	method get_neuron i = neurones.(i)
	method set_neuron i n = neurones.(i) <- n
	method get_out_to i =
		let r = ref 0. in
		Array.iter (fun n -> r := !r +. (n#get_out i)) neurones;
		!r
	method reset =
		Array.iter (fun n -> n#reset) neurones
	method to_string =
		let s = ref "" in
		Array.iter (fun n -> s := (if !s = "" then "" else Stdlib.string_of_char separator) ^ (n#to_string)) neurones;
		!s
	method nb_neur = Array.length neurones
    end

let string_to_layer s =
	let ars = Stdlib.split_string s separator in
	let neur = Array.init (Array.length ars) (fun i -> Neuron.string_to_neuron (!(ars.(i)))) in
	let lay = new layer (Array.length neur) ((neur.(0))#nb_out) in
	for i = 0 to lay#nb_neur do
		lay#set_neuron i (neur.(i))
	done;
	lay
