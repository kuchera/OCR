val separator : char
class layer :
  int ->
  int ->
  object
    val neurones : Neuron.neuron array
    method get_neuron : int -> Neuron.neuron
    method get_out_to : int -> float
    method nb_neur : int
    method reset : unit
    method set_neuron : int -> Neuron.neuron -> unit
    method to_string : string
  end
val string_to_layer : string -> layer
