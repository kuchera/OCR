let dirdetect = "files/network/"

let f =
	(*new Graph.graph 0.5 [|2;2;1|]
	g#training [|1.;1.|] [|0.|]
	Printf.printf "%f\n" (g#get_out [|1.;1.|]).(0)*)
	let s = Iostream.read_file (dirdetect ^ "detected/1.mat") in
	let a = Stdlib.string_to_intmat s '|' ';' in
	Array.iter (fun e ->
		Array.iter (fun ee -> Printf.printf "%d " ee) e;
		Printf.printf "\n") a
