(* Entry point of the program *)
(* using : Iostream *)



(*=============== TESTS =============== *)
let _ = 
	begin
		if Array.length Sys.argv == 4 then
			let f = new Iostream.file (Sys.argv.(1)) in
			try
				Printf.printf "%s\n" (f#read_line (int_of_string (Sys.argv.(2))))
			with e -> ()
		else if Array.length Sys.argv == 3 then
			let f = new Iostream.file (Sys.argv.(1)) in
			if f#print_string (Sys.argv.(2)) = 0 then 
				Printf.printf "Done.\n"
			else
				Printf.printf "Error.\n"
		else if Array.length Sys.argv == 2 then
			let f = new Iostream.file (Sys.argv.(1)) in
			let a = f#read_lines in
			Array.iter (function s -> Printf.printf "%s\n" !s) a
		else
			(Printf.printf "%s FILENAME TEXT\n" Sys.argv.(0);
			Printf.printf "%s FILENAME LINE CHARACTER\n" Sys.argv.(0);
			Printf.printf "%s FILENAME\n" Sys.argv.(0))
	end
