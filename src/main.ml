(* Entry point of the program *)

let print s = Printf.printf "%s\n" s

let print_file f = print (Iostream.read_file f)

let get_entry () =
	let s = ref "" in
	while !s = "" do
		s := read_line ()
	done;
	!s

let getinfile () = 
	print "Enter the input file name : ";
	get_entry ()

let getoutfile () = 
	print "Enter the output file name : ";
	get_entry ()

let display f =
	Printf.printf "Displaying %s...\n" f;
	let return = Sys.command ("./interface "^f) in
	Printf.printf "Returned code : %d\n" return

let exec_fil inf outf =
	Printf.printf "Applying filtres to \"%s\"...\n" inf;
	let return = Sys.command ("./filtres "^inf^" "^outf) in
	Printf.printf "Returned code : %d\n" return;
	if return = 0 then
		Printf.printf "Image saved in \"%s\".\n" outf

let exec_noise inf outf =
	Printf.printf "Removing noise from \"%s\"...\n" inf;
	let return = Sys.command ("./filtres noise "^inf^" "^outf) in
	Printf.printf "Returned code : %d\n" return;
	if return = 0 then
		Printf.printf "Image saved in \"%s\".\n" outf

let exec_bin inf outf =
	Printf.printf "Binarising \"%s\"...\n" inf;
	let return = Sys.command ("./filtres binarize "^inf^" "^outf) in
	Printf.printf "Returned code : %d\n" return;
	if return = 0 then
		Printf.printf "Image saved in \"%s\".\n" outf

let exec_rot inf outf =
	Printf.printf "Rotating \"%s\"...\n" inf;
	let return = Sys.command ("./rotation "^inf^" "^outf) in
	Printf.printf "Returned code : %d\n" return;
	if return = 0 then
		Printf.printf "Image saved in \"%s\".\n" outf

let exec_det inf outf =
	Printf.printf "Detecting text zones \"%s\"...\n" inf;
	let return = Sys.command ("./detection draw "^inf^" "^outf) in
	Printf.printf "Returned code : %d\n" return;
	if return = 0 then
		Printf.printf "Image saved in \"%s\".\n" outf

let _ =
try
	print_file "files/header";

	let inf = ref (getinfile ())
	and outf = ref (getoutfile ()) in

	let s = ref "" in
	while !s <> "exit" do
		print_string " >";
		s := read_line ();
		match !s with
		| "noise" -> exec_noise !inf !outf
		| "bin"   -> exec_bin !inf !outf
		| "fil"   -> exec_fil !inf !outf
		| "rot"   -> exec_rot !inf !outf
		| "det"   -> exec_det !inf (get_entry ())
		| "dis"   -> display !outf
		| "c"     -> ignore (Sys.command (get_entry ()))
		| "help"  -> print_file "files/help"
		| "exit"  -> print "Exiting..."
		| "in"    -> inf := getinfile ()
		| "out"   -> outf := getoutfile ()
		| _       -> print "Type \"help\' for help."
	done;
	
	exit 0
with e -> 
	print "The program encountered an error.";
	(match e with
		| Failure s -> print s
		| _ -> ());
	exit 1	
