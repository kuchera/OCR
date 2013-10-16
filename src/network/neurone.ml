(* Modules Neurone *)

class neuron text nb_out =
    object (this)
        val label = text
        val value = 0.
        val out = Array.make nb_out (0,0.)
        
        method reset = value <- 0.
        method add v = value <- value +. v
        method add_weight e x =
            let (o,p) =  out.(e) in
            out.(e) <- (o,p +. x)
        method reset_weight e =
            let (o,p) = out.(e) in
            out.(e) <- (o,0.)
    end
