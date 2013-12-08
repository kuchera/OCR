
let _ = 
	if Array.length Sys.argv = 1 then 
		()
	else if Sys.argv.(1) = "learn" then
		((if Array.length Sys.argv = 3 then
			ignore (Iostream.write_file "files/network/iter" Sys.argv.(2)));
		Learn.learn ())
	else
		(Printf.printf "%s - read\n" Sys.argv.(0);
		Printf.printf "%s learn [iter] - launches the learning phase (iter is optionnal)\n" Sys.argv.(0))
