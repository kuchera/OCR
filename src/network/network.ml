(* Module Network *)

let print s = Printf.printf "%s\n" s

let _ = 
let g = ref (new Graph.graph [|2;5;3|]) in
let s = (!g)#to_string in
g := Graph.string_to_graph s;
print ((!g)#to_string)
