      (* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
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



let save img = Sdlvideo.save_BMP img "img.bmp";;



(*let save_binary_img img w h = let surface = Sdlvideo.create_RGB_surface_format img [] w h in
		 for i = 0 to w do 
			for j = 0 to h do
			let color = Sdlvideo.get_pixel_color img i j in
			Sdlvideo.put_pixel_color surface i j color
			done
		done

*)

(* main *)
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
    save (binary img);
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
     (* on quitte *)
      exit 0
  end
 
let _ = main ()
