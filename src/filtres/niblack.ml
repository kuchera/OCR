let level (r,g,b) = ( 0.3*.(float_of_int r)+.0.59*.(float_of_int g)+.0.11*.(float_of_int b))

let color2grey (r,g,b) = let aux = int_of_float(level (r,g,b)) in
                         (aux,aux,aux)


let matrix w h = Array.make_matrix w h (0,0,0)

let rec average l = match l with 
            |[] -> 0.
            |(r,g,b)::l1 -> level (r,g,b) +. average l1

let final_average l = (average l) /. float_of_int((List.length l) - 1)

let get_arround matrix x y = 
    let l = ref [] in 
    for i = x-4 to x+4 do
        for j = y-2 to y+2 do
            try l := (matrix.(i).(j)) :: (!l) ; () with e -> () 
        done
    done;
    (!l)



let carre x = x *. x 

let rec variance l grey_average = 
                    match l with 
                    |[] -> 0. 
                    |(r,g,b)::l1 -> (carre ((level (r,g,b)) -. grey_average)) +.
                    variance l1 grey_average


let ecart_type l average = sqrt ((variance l average) /.
(float_of_int(List.length l) -. 1.))

let img_to_matrix img =
    let (w,h,_) = Sdlvideo.surface_dims img in 
    let mat = matrix w h in
    for i = 0 to w-1 do
    for j = 0 to h-1 do 
        let (r,g,b) = Sdlvideo.get_pixel_color img i j in
        mat.(i).(j) <- (r,g,b)
        done
    done;
mat    

let seuil matrix i j = 
                      let arround = get_arround matrix i j in 
                      let grey_average = final_average arround in
                      let ecart = ecart_type arround grey_average in
                       (grey_average *. (1. +. 0.2 *.((ecart /. 128.) -. 1.)))
                       


let binary img = 
  let (w,h,_) = (Sdlvideo.surface_dims img) in
	let matrix = img_to_matrix img in 
	let surface = Sdlvideo.create_RGB_surface_format img [] w h in
      for i = 0 to (w-1) do
        for j = 0 to (h-1) do
            let seuil = seuil matrix i j in
          let (r,g,b) = Sdlvideo.get_pixel_color img i j  in
	let grey = level (r,g,b) in
	if (grey > seuil) then
          Sdlvideo.put_pixel_color surface i j (255,255,255)
	else 
          Sdlvideo.put_pixel_color surface i j (0,0,0)
	done;
      done;
	surface

let binarize input output = 
        let im = Sdltools.load_image input in
        Sdltools.save output (binary im)
