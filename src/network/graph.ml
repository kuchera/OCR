(* Module Graph *)

let separator = '/'

class graph param = (* graph : liste du nombre de neurones par couche en commencant pas la couche la plus basse *)
object (this)
	val layers =
		let l = Array.length param in
		Array.init l (fun i ->
			new Layer.layer (param.(i)) (if i >= l-1 then 1 else param.(i+1)))
	
	method get_layers = layers
	method get_layer i = layers.(i)
	method set_layer i l = layers.(i) <- l
	method get_neuron l n = (layers.(l))#get_neuron n
	method reset = Array.iter (function e -> e#reset) layers
	method to_string = 
		let s = ref "" in
		Array.iter (function l -> s := !s ^ (if !s = "" then "" else Stdlib.string_of_char separator) ^ (l#to_string)) layers;
		!s
end

let string_to_graph s =
	let ar = Stdlib.split_string s separator in
	let ar = Array.init (Array.length ar) (function i -> 
		Layer.string_to_layer !(ar.(i))) in
	let param = Array.init (Array.length ar) (function i -> 
		(ar.(i))#nb_neur) in
	let g = new graph param in
	Array.iteri (fun i e -> g#set_layer i e) ar;
	g
