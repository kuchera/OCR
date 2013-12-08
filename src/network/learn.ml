let linf =     "files/network/lin"
let iterf =    "files/network/iter"
let dir =      "files/network/detected/"
let datadir =  "files/network/data/"

	(*new Graph.graph 0.5 [|2;2;1|]
	g#training [|1.;1.|] [|0.|]
	Printf.printf "%f\n" (g#get_out [|1.;1.|]).(0)*)

let show i =
	let s = Iostream.read_file (dir^(string_of_int i)^".mat") in
	let a = Stdlib.string_to_intmat s '|' ';' in
	Array.iter (fun e ->
		Array.iter (fun ee -> Printf.printf "%d " ee) e;
		Printf.printf "\n") a
let read i =
	Iostream.read_file (dir ^ (string_of_int i) ^ ".mat")
let showmat m = 
	Array.iter (fun e ->
		Array.iter (fun ee ->
			Printf.printf "%d " ee
		) e;
		Printf.printf "\n"
	) m;
	Printf.printf "\n"

let learn () =
	let size = int_of_string (Iostream.read_file (dir^"size")) in
	let count = int_of_string (Iostream.read_file (dir^"count")) in
	let iter = int_of_string (Iostream.read_file iterf) in
	let lin = Iostream.read_file linf in
	Printf.printf "expected_in    %s\n" lin;
	Printf.printf "char_count     %d\n" count;
	Printf.printf "mat_size       %dx%d\n" size size;
	Printf.printf "nb_iter        %d\n" iter;
	let g = new Graph.graph 0.5 [|size * size ; size*size ; count |] in
	let a = Array.init count (fun i -> Stdlib.string_to_intmat (read (i+1)) '|' ';') in
	let a = Array.init count (fun i -> Stdlib.intmat_to_floatarr a.(i) ) in
	let out = Array.make count 0. in

	for it=0 to iter do
		for i=0 to count - 1 do
			out.(i) <- 1.;
			g#training a.(i) out;
			out.(i) <- 0.
		done;
	done;

	ignore (Iostream.write_file (datadir^"chars") lin);
	ignore (Iostream.write_file (datadir^"nbin") (string_of_int (size*size)));
	ignore (Iostream.write_file (datadir^"graph.dat") g#to_string)
