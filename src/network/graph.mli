val separator : char
class graph :
  float ->
  int array ->
  object
    val mutable _layers : Layer.layer array
    val mutable _output : Layer.layer
    method get_layer : int -> Layer.layer
    method get_max_out : float array -> float
    method get_out : float array -> float array
    method set_layers : Layer.layer array -> unit
    method to_string : string
    method training : float array -> float array -> unit
  end
val parse : string -> graph
