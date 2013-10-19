(* Module Layer *)

class layer nb_neur nb_out =
    object
        val neurones = Array.make nb_neur (ref (new Neuron.neuron "" nb_out))
    end
