let file = "files/network/data.dat"

let test g = 
	Printf.printf "%f\n" (g#get_out [|0.;0.|]).(0);
	Printf.printf "%f\n" (g#get_out [|1.;0.|]).(0);
	Printf.printf "%f\n" (g#get_out [|0.;1.|]).(0);
	Printf.printf "%f\n" (g#get_out [|1.;1.|]).(0)

let _ =
if Array.length (Sys.argv) <= 1 then
	Printf.printf "r / w\n"
else if Sys.argv.(1) = "w" then
	(let g = new Graph.graph 0.5 [|2;2;1|] in
	for i=0 to 50000 do
		g#training [|0.;0.|] [|0.|];
		g#training [|1.;0.|] [|1.|];
		g#training [|0.;1.|] [|1.|];
		g#training [|1.;1.|] [|0.|]
	done;
	test g;
	ignore (Iostream.write_file file (g#to_string)))
else if Sys.argv.(1) = "r" then
	(let g = Graph.parse (Iostream.read_file file) in
	test g;)
