(* Entry point *)

let _ = 
	if Array.length (Sys.argv) <= 1 then
	Printf.printf "%s filename" (Sys.argv.(0))
	else
	Sdltools.sdl_init ();
	let img = Sdltools.load_image (Sys.argv.(1)) in
	let (w,h) = Sdltools.get_img_dim img in
	let screen = Sdltools.init_display w h in
	Sdltools.show_img img screen;
	Sdltools.wait_key
