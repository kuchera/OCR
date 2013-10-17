

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)


(* Init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst


let img_to_matrix img = 
	let (w,h,_) = Sdlvideo.surface_dims img in  
	let matrix = Array.make_matrix w h (0,0,0) in
	for i = 0 to w-1 do 
		for j = 0 to h-1 do 
		let (r,g,b) = Sdlvideo.get_pixel_color img i j in
		matrix.(i).(j) <- (r,g,b)
		done;
	done;
	matrix
	
let rec insert (r,g,b) = function
  | [] -> [(r,g,b)]
  | (r1,g1,b1) :: l -> 	if ((r+g+b)/3) < ((r1+g1+b1)/3) then (r,g,b) :: (r1,g1,b1) :: l 
		else (r1,g1,b1) :: insert (r,g,b) l;;


let rec tri = function
   | [] -> [] 
   | x :: l -> insert x (tri l)


let eight_pixels matrix i j = 
	if (i = 0 && j = 0) then 
		tri [	matrix.(i+1).(j); 
			matrix.(i+1).(j+1);
			matrix.(i).(j+1)]
	else if (i = 0) then 
		tri [	matrix.(i).(j+1);
			matrix.(i+1).(j-1);
			matrix.(i+1).(j);
			matrix.(i+1).(j+1);
			matrix.(i).(j-1)]
	else if (j = 0) then
		tri [ 	matrix.(i-1).(j);
			matrix.(i-1).(j+1);
			matrix.(i).(j+1);
			matrix.(i+1).(j+1);
			matrix.(i+1).(j)]
	else if ((i = (Array.length  matrix)-2) && (j = (Array.length matrix)-2)) then
		tri [ 	matrix.(i-1).(j);
			matrix.(i-1).(j-1);
			matrix.(i).(j-1)]
	else if (i = (Array.length matrix)-2) then 
		tri [	matrix.(i).(j-1);
			matrix.(i-1).(j-1);
			matrix.(i-1).(j);
			matrix.(i-1).(j+1);
			matrix.(i).(j+1)]
	else if (j = (Array.length matrix) -2) then
		tri [	matrix.(i-1).(j);
			matrix.(i-1).(j-1);
			matrix.(i).(j-1);
			matrix.(i+1).(j-1);
			matrix.(i+1).(j)]
	else 
		tri [	matrix.(i-1).(j-1);
			matrix.(i).(j-1);
			matrix.(i+1).(j-1);
			matrix.(i+1).(j);
			matrix.(i+1).(j+1);
			matrix.(i).(j+1);
			matrix.(i-1).(j+1);
			matrix.(i-1).(j)]
	


let rec list_ieme l i = match l with 
    | e::l -> if i <= 0 then e else list_ieme l (i-1)
    | [] -> failwith "Depassement de liste."

let median l = list_ieme l (((List.length l) / 2) - 2)

let erase matrix img surface w h = 
	for i = 0 to w-2 do 
		for j = 0 to h-2 do
	let l = eight_pixels matrix i j in
	let med = median l in   
	 
	Sdlvideo.put_pixel_color surface i j med
		done
	done

let save img = Sdlvideo.save_BMP img "sans_bruit.bmp"

let main () =
  begin
  (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
	let surface = Sdlvideo.create_RGB_surface_format img [] w h in
	let matrix = img_to_matrix img in 
	erase matrix img surface w h ;
 (* on affiche l'image *)
      show surface display;
	save surface;
      (* on attend une touche *)
      wait_key ();
     (* on quitte *)
      exit 0
  end


let _ = main ()
