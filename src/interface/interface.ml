(* Entry point *)

let _ = 
	if Array.length (Sys.argv) <= 1 then
		Printf.printf "%s filename - Show all the images given in the parameters.\n" (Sys.argv.(0))
	else
	begin
		Sdltools.sdl_init ();
		for i = 1 to Array.length (Sys.argv) - 1 do
			Sdltools.set_window_name (Sys.argv.(i)) "Images display";
			let img = Sdltools.load_image (Sys.argv.(i)) in
			let (w,h) = Sdltools.get_img_dim img in
			let screen = Sdltools.init_display w h in
			Sdltools.show_img img screen;
			Sdltools.wait_key ()
		done
	end
