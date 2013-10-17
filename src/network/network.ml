(* Module Network *)

let print s = Printf.printf "%s\n" s

let _ = 
try
	if Array.length (Sys.argv) > 1 then
		if Sys.argv.(1) = "export" then
			let n = new Neuron.neuron "Label" 1 in
			ignore (Iostream.write_file "data" (n#to_string));
			()
		else if Sys.argv.(1) = "import" then
			let n = Neuron.string_to_neuron (Iostream.read_file "data") in
			print (n#to_string);
			()
		else
			print "import/export"
	else
		print "import/export";
	exit 0
with e -> match e with
	| Failure (s) -> print "Error" ; print s ; exit 1
	| _ -> exit 1
