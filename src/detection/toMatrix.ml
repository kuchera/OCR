let is_black (r, g, b) = match (r, g, b) with
        |( 0, 0, 0) -> true
        |_ -> false

let rectToMatrix img (x,y,larg,haut) = 
	let matrix = Array.init larg (fun _ -> Array.init haut (fun _ -> 0) ) in
	let (w, h) = Sdltools.get_img_dim img in
	for i = x to x+larg-1 do
		for j = y to y+haut-1 do
			if ( i > 0 && i < w && j > 0 && j < h) then
			begin
				let c = Sdlvideo.get_pixel_color img i j in
				if (is_black c) then
				begin
					matrix.(i-x).(j-y) <- 1;
				end;
			end;
		done;
	done;
	matrix

let redimMatrix matrix =
	let new_matrix = Array.init 100 (fun _ -> Array.init 100 (fun _ -> 0) ) in
	let coefX = float (Array.length matrix) /. 100. in
	let coefY = float (Array.length matrix.(1)) /. 100. in
	for i = 0 to 99 do
		for j = 0 to 99 do
			new_matrix.(i).(j) <- matrix.((int_of_float (float(i) *. coefX))).((int_of_float (float(j) *. coefY)))
		done;
	done;
	new_matrix

(*fction a utiliser*)
let getMatrix imgPath e =
	Sdltools.sdl_init ();
	let img = Sdlloader.load_image imgPath in
        redimMatrix (rectToMatrix img e)
