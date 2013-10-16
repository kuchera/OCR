(* Entry point of the program *)
(* using : Iostream *)

let print s = Printf.printf "%s\n" s

let _ =
    begin
	Sdltools.sdl_init ();
	let screen = Sdltools.init_display 200 150 in
	Sdltools.wait_key ();
	print "Done..."
    end
