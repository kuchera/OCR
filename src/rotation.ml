(*Rotation*)

let pi = 3.141592654

let x_to_xrot x y theta =float(x)*.cos(theta) +. float(y)*.sin(theta)

let y_to_yrot x y theta = float(-x)*.sin(theta) +.float(y)*.cos(theta)

let x_xrot x y x_center y_center theta = (x_center)
  +. ((x-.x_center)*.(cos theta)) 
  -. ((y-.y_center)*.(sin theta))

let y_yrot x y x_center y_center theta = (y_center)
  +. ((x-.x_center)*.(sin theta))
  +. ((y-.y_center)*.(cos theta))

let create_matrix_rot img =
  let (w,h) = get_dims img in
  Array.make_matrix w h (255,255,255)
(* Crée une matrice de rotation *)

let rotation img theta =
  let theta_rad = theta *. pi /. 180. in
  let matrix_rotate = create_matrix_rot img in 
  let (w,h) = get_dims img in
  for x=0 to w-1 do
    for y=0 to h-1 do
      let xrot =int_of_float(x_to_xrot x y theta_rad) in
      let yrot =int_of_float(y_to_yrot x y theta_rad) in
      if xrot>=0 && xrot <= w-1 && yrot >=0 && yrot <= h-1 then 
       matrix_rotate.(xrot).(yrot) <- Sdlvideo.get_pixel_color img x y
    done;
  done;
  matrix_rotate;;
(* Effectue la rotation sur la matrice de rotation*)

let new_rot img theta =
  let (w,h) = get_dims img in
  let matrix_rotate = create_matrix_rot img in
  let theta_rad = (theta) *. pi /. 180. in
  let (x_center,y_center) = ((float (w))/.2.,(float (h))/.2.) in
  for i = 0 to w-1 do
    for j= 0 to h-1 do
      let x = float i in
      let y = float j in
      let xrot = int_of_float(x_xrot x y x_center y_center theta_rad) in
      let yrot =int_of_float(y_yrot x y x_center y_center theta_rad) in
      if xrot>=0 && xrot <= w-1 && yrot >=0 && yrot <= h-1 then
      matrix_rotate.(xrot).(yrot) <- Sdlvideo.get_pixel_color img i j
    done;
  done;
matrix_rotate;;

let apply_rot img ang =
  let (w,h) = get_dims img in
  let mat = Array.init w (fun _ -> Array.make h (255,255,255)) and pi = 4.0 *. (atan 1.0)
  and x0 = (float_of_int w)/.2. and y0 = (float_of_int h)/.2. in
  let c = cos(ang/.180.*.pi) and s = sin(ang/.180.*.pi) in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let x = int_of_float (x0+.((float_of_int i)-.x0)*.c-.((float_of_int j)-.y0)*.s)
      and y = int_of_float (y0+.((float_of_int i)-.x0)*.s+.((float_of_int j)-.y0)*.c) in
      if x >= 0 && x < w && y >= 0 && y < h then
        mat.(i).(j) <- Sdlvideo.get_pixel_color img x y
    done
  done;
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      Sdlvideo.put_pixel_color img i j (mat.(i).(j))
    done;
  done;
img;;
    
let matrix_to_img img matrix =
  let (w,h) = get_dims img in
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  for x=0 to w-1 do
    for y=0 to h-1 do
      Sdlvideo.put_pixel_color dst x y (matrix.(x).(y))
    done;
  done;
  dst;;
(* Convertit la matrice en image *)

let max_matrix tab =
  let dim tab =
    let n = Array.length tab in
    if n = 0 then (0, 0) else (n,Array.length tab.(0)) in
  let (w,h) = dim tab in
  let rho_max = ref 0 in
  let teta = ref 0 in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      if tab.(x).(y) > !rho_max then
	begin 
	  rho_max := tab.(x).(y);
	  teta := y;
	end
    done;
  done;
  !teta;;
(* Permet d'obtenir le maximum d'une matrice *)

let hough img =
  let (w,h) = get_dims img in
  let dist = int_of_float (sqrt (float_of_int(w*w + h*h))) in
  let matrix = Array.make_matrix dist 91 0 in
  let rho_float = ref 0. in
  let rho_int = ref 0 in
  for i=0 to (w-1) do
    for j=0 to (h-1) do
      if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	begin
	  for teta = 0 to 90 do
	    let ang = (float teta)*. pi /. 180. in
	    rho_float := (float i) *. cos(ang) +. (float j) *. sin(ang);
            rho_int := int_of_float(!rho_float);
	    (*if !rho_int < 0 then rho_int := (-1)*(!rho_int);*)
	    matrix.(!rho_int).(teta) <- matrix.(!rho_int).(teta) + 1;
	  done;
	end
    done;
  done;
  max_matrix matrix;;


let best_quality img = 
  let (w,h) = get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let pix = Sdlvideo.get_pixel_color img i j in
      let next_pix = Sdlvideo.get_pixel_color img (i+1) (j) in
      let prev_pix = Sdlvideo.get_pixel_color img (i+1) (j) in
      if pix = (255,255,255) && next_pix < (128,128,128) && prev_pix < (128,128,128) then
	Sdlvideo.put_pixel_color img i j (0,0,0);
    done
  done

let final img =
  let angle_int = (hough img) - 90 in
  let angle = float_of_int angle_int  in
  apply_rot img angle;
  (*best_quality img;*)
img;;
