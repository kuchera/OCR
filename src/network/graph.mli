val separator : char
class graph :
  int array ->
  object
    val layers : Layer.layer array
    method get_layer : int -> Layer.layer
    method get_layers : Layer.layer array
    method get_neuron : int -> int -> Neuron.neuron
    method reset : unit
    method set_layer : int -> Layer.layer -> unit
    method to_string : string
  end
val string_to_graph : string -> graph
