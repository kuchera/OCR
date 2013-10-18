(*fonctions à coder*) 
let level (r,g,b) = ( 0.3*.(float_of_int r)+.0.59*.(float_of_int
        g)+.0.11*.(float_of_int b))/.3. ;;

let color2grey (r,g,b) = let aux = int_of_float(level (r,g,b)) in
                         (aux,aux,aux)
(* initialise la matrice *)
let matrix w h = Array.make_matrix w h (0,0,0)

(* Calcul le seuil, renvoie un int ref *)
let seuil img w h = let s = ref 0 in
		for i = 0 to w-1 do
		for j = 0 to h-1 do 
          let (r,g,b) = Sdlvideo.get_pixel_color img i j  in
			s := !s + int_of_float(level (r,g,b))
		done;
		done; 
		s := (!s / (w*h));
		s	

(* Image en NOIR et BLANC *)
let binary img = 
  let (w,h,_) = (Sdlvideo.surface_dims img) in
	let seuil = seuil img w h in
	let surface = Sdlvideo.create_RGB_surface_format img [] w h in
      for i = 0 to (w-1) do
        for j = 0 to (h-1) do
          let (r,g,b) = Sdlvideo.get_pixel_color img i j  in
	let grey = int_of_float(level (r,g,b)) in
	if (grey >= !seuil) then
          Sdlvideo.put_pixel_color surface i j (255,255,255)
	else 
          Sdlvideo.put_pixel_color surface i j (0,0,0)
	done;
      done;
	surface

let binarize input output = 
    let img = Sdltools.load_image input in
    Sdltools.save output (binary img)
