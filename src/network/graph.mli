class graph :
  int ->
  float ->
  int array ->
  object
    val mutable _layers : Layer.layer array
    val mutable _output : Layer.layer
    method get_out : float array -> float array
    method training : float array -> float array -> unit
  end
