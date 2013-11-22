class layer :
  int ->
  int ->
  object
    val mutable _neurons : Neuron.neuron array
    method adjust_weight : float -> unit
    method get : int -> Neuron.neuron
    method get_out : float array
    method length : int
    method serror : int -> float -> unit
    method set : int -> Neuron.neuron -> unit
    method set_allin : float array -> unit
    method set_in : int -> float array -> unit
    method sum_adj : int -> float
  end
