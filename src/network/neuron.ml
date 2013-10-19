(* Modules Neurone *)

let sep1 = ':'
let sep2 = ';'

class neuron text nb_out =
    object (this)
        val label:string = text
        val mutable value = 0.
        val out = Array.make nb_out (0.)
        
	method text = label
        method reset = value <- 0.
        method add v = value <- value +. v
        method add_weight e x =
            out.(e) <- out.(e) +. x
        method reset_weight e =
            out.(e) <- 0.
	method valueof e = out.(e)
	method to_string = 
		let l = ref [] in
		l :=  (Stdlib.floatarray_to_string out sep1) :: !l;
		l := (string_of_float value) :: !l;
		l := text :: !l;
		Stdlib.stringlist_to_string (!l) sep2
	method nb_out = Array.length out
	method get_out i = out.(i)
    end

let string_to_neuron s = 
	try
	let a = Stdlib.split_string s sep2 in
	let l = Stdlib.string_to_floatarray !(a.(2)) sep1 in
	let n = new neuron !(a.(0)) (Array.length l) in
		n#add (float_of_string !(a.(1)));
		Array.iteri (fun i x -> n#add_weight i x) l;
		n
	with e -> failwith "Cannot parse neuron."
