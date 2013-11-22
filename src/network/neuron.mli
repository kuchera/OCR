class neuron :
  int ->
  object
    val mutable _adjustement : float
    val mutable _error : float
    val mutable _inputs : float array
    val mutable _value : float
    val mutable _weights : float array
    method adjust_weight : float -> unit
    method error : float
    method out : float
    method serror : float -> unit
    method set_input : float array -> unit
    method weight : int -> float
  end
