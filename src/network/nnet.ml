let dir   = "files/network/detected/"
let chars = "files/network/data/chars"
let graph = "files/network/data/graph.dat"

let run () =
	let chars = Iostream.read_file chars in
	let count = int_of_string (Iostream.read_file (dir^ "count")) in
	let inputarrs = Array.init count (fun i -> Stdlib.intmat_to_floatarr (Stdlib.string_to_intmat (Iostream.read_file (dir ^ (string_of_int i) ^ ".mat")) '|' ';')) in
	let out = ref "" and outlog = ref "" in
	let net = Graph.parse (Iostream.read_file graph) in
	for i=0 to count - 1 do
		let (mi,mv) = net#get_max_out inputarrs.(i) in
		out := !out ^ (Stdlib.string_of_char chars.[mi]);
		outlog := !outlog ^ (Stdlib.string_of_char chars.[mi]) ^ " (" ^ (string_of_float mv) ^ ")\n"
	done;
	ignore (Iostream.write_file "files/network/out.txt" !out);
	ignore (Iostream.write_file "files/network/outlog.txt" !outlog)

let _ = 
	if Array.length Sys.argv = 1 then 
		run ()
	else if Sys.argv.(1) = "learn" then
		((if Array.length Sys.argv = 3 then
			ignore (Iostream.write_file "files/network/iter" Sys.argv.(2)));
		Learn.learn ())
	else
		(Printf.printf "%s - read\n" Sys.argv.(0);
		Printf.printf "%s learn [iter] - launches the learning phase (iter is optionnal)\n" Sys.argv.(0))
