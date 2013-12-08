let is_black (r, g, b) = match (r, g, b) with
        |( 0, 0, 0) -> true
        |_ -> false

let rectToMatrix img (x,y,haut,larg) = 
	let matrix = Array.init larg (fun _ -> Array.init haut (fun _ -> 0) ) in
	for i = x to x+larg-1 do
		for j = y to y+haut-1 do
			let c = Sdlvideo.get_pixel_color img i j in
			if (is_black c) then
				matrix.(i-x).(j-y) <- 1;
			Sdlvideo.put_pixel_color img i j (0, 0, 255);
		done;
	done;
	matrix

let redimMatrix size matrix =
	let new_matrix = Array.init size (fun _ -> Array.init size (fun _ -> 0) ) in
	let coefX = float (Array.length matrix) /. (float (size)) in
	let coefY = float (Array.length matrix.(1)) /. (float (size)) in
	for i = 0 to size-1 do
		for j = 0 to size-1 do
			new_matrix.(i).(j) <- matrix.((int_of_float (float(i) *. coefX))).((int_of_float (float(j) *. coefY)))
		done;
	done;
	new_matrix

(*fonction a utiliser*)
let getMatrix size imgPath e =
	Sdltools.sdl_init ();
	let img = Sdlloader.load_image imgPath in
	redimMatrix size (rectToMatrix img e);
	(*let (w, h) = Sdltools.get_img_dim img in
	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
        Sdltools.show_img img display;
        Sdltools.wait_key();
	ar2send*)
