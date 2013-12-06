let separator = ';'

let random = 
	Random.self_init ();
	fun () -> (if Random.bool () then 1. else -1.) *. (Random.float 0.5)

let sigmoid x = 1. /. (exp (-. x) +. 1.)

class neuron nb_in =
    object (this)
        val mutable _value = 0.
	val mutable _adjustement = 0.
	val mutable _inputs = Array.make nb_in 0.
	val mutable _weights = Array.init nb_in (fun _ -> random ())
	val mutable _error = 0.

	method set_adjustement a = _adjustement <- a
	method out = 
		let out = ref 0. in
		for i=0 to Array.length _inputs - 1 do
			out := !out +. _inputs.(i) *. _weights.(i)
		done;
		sigmoid (!out +. _adjustement)
	method set_input t =
		Array.iteri (fun i e -> _inputs.(i) <- e) t
	method adjust_weight speed =
		Array.iteri (fun i e -> _weights.(i) <- e +. speed *. _error *. _inputs.(i)) _weights;
		_adjustement <- _adjustement +. speed *. _error
	method error = _error
	method serror e = _error <- e
	method weight i = _weights.(i)
	method set_weight t = _weights <- t
	method to_string = (string_of_float _adjustement) ^ (Stdlib.string_of_char separator) ^ (Stdlib.floatarray_to_string _weights separator)
    end

let parse str = 
	let a = Stdlib.string_to_floatarray str separator in
	let n = new neuron (Array.length a - 1) in
	n#set_adjustement a.(0);
	n#set_weight (Array.init (Array.length a - 1) (fun i -> a.(i+1)));
	n
