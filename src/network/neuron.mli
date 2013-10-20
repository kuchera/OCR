val sep1 : char
val sep2 : char
class neuron :
  string ->
  int ->
  object
    val mutable label : string
    val out : float array
    val mutable value : float
    method add : float -> unit
    method add_weight : int -> float -> unit
    method get_out : int -> float
    method nb_out : int
    method reset : unit
    method reset_weight : int -> unit
    method set_text : string -> unit
    method text : string
    method to_string : string
    method valueof : int -> float
  end
val string_to_neuron : string -> neuron
