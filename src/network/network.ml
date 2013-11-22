let _ =
	let g = new Graph.graph 2 0.5 [|2;2;1|] in
	for i=0 to 50000 do
		g#training [|0.;0.|] [|0.|];
		g#training [|1.;0.|] [|1.|];
		g#training [|0.;1.|] [|1.|];
		g#training [|1.;1.|] [|0.|]
	done;
	Printf.printf "%f\n" (g#get_out [|0.;0.|]).(0);
	Printf.printf "%f\n" (g#get_out [|1.;0.|]).(0);
	Printf.printf "%f\n" (g#get_out [|0.;1.|]).(0);
	Printf.printf "%f\n" (g#get_out [|1.;1.|]).(0)
