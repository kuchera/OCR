let lin = "files/network/lin"
let dir = "files/network/detected/"

	(*new Graph.graph 0.5 [|2;2;1|]
	g#training [|1.;1.|] [|0.|]
	Printf.printf "%f\n" (g#get_out [|1.;1.|]).(0)*)

let learn () =
	let size = int_of_string (Iostream.read_file (dir^"size")) in
	Printf.printf "%d %s\n" size (Iostream.read_file (dir^"count"))
