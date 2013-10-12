let rec print = function
	| e::l -> Printf.printf "%s\n" e; print l
	| [] -> ()

let _ = 
	begin
		if Array.length Sys.argv >= 3 then
			let f = new Iostream.file (Sys.argv.(1)) in
			if f#writestring (Sys.argv.(2)) = 0 then 
				Printf.printf "Done.\n"
			else
				Printf.printf "Error.\n"
		else if Array.length Sys.argv == 2 then
			let f = new Iostream.file (Sys.argv.(1)) in
			print !(f#readlines)
		else
			(Printf.printf "%s FILENAME TEXT\n" Sys.argv.(0);
			Printf.printf "%s FILENAME\n" Sys.argv.(0))
	end
