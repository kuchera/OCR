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
        g)+.0.11*.(float_of_int b)) ;;

let color2grey (r,g,b) = let aux = int_of_float(level (r,g,b)) in
                         (aux,aux,aux)


let image2gray img =
  let (w,h,_) = (Sdlvideo.surface_dims img) in

      for i = 0 to (w-1) do
        for j = 0 to (h-1) do
          let (r,g,b) = Sdlvideo.get_pixel_color img i j  in
          let grey =color2grey (r,g,b) in
          Sdlvideo.put_pixel_color img i j grey
	done
      done
;;
(* Initialisation tableau int 2 dimensions *)
let matrix n m init =
  let result = Array.make n (Array.make m init) in
    for i = 1 to n - 1 do
      result.(i) <- Array.make m init
    done;
    result
(*##########################rotation et recherche d'angle de rotation##########################################*)
(*Arrondir un nombre flottant pour plus de précision*)

let round a = 
  if a >= 0. then
    if (a+.0.5) >= float_of_int((int_of_float a) + 1) then (int_of_float a) + 1
    else int_of_float a
  else
    let b = -.a in
      if (b+.0.5) >= float_of_int((int_of_float b) + 1) then -((int_of_float b) + 1)
      else int_of_float a

(*Applique la rotation d'une image à partir d'un certain angle*)

let apply_rot img ang =
  let (w,h) = get_dims img in
  let mat = matrix w h (255,255,255) and pi = 4.0 *. (atan 1.0) and x0 = (float_of_int w)/.2. and y0 = (float_of_int h)/.2. in
    let c = cos(ang/.180.*.pi) and s = sin(ang/.180.*.pi) in
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          let x = round (x0+.((float_of_int i)-.x0)*.c-.((float_of_int j)-.y0)*.s)
          and y = round (y0+.((float_of_int i)-.x0)*.s+.((float_of_int j)-.y0)*.c) in
            if x >= 0 && x < w && y >= 0 && y < h then
              mat.(i).(j) <- Sdlvideo.get_pixel_color img x y
        done
      done;
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          Sdlvideo.put_pixel_color img i j (mat.(i).(j))
        done
      done;
      img;;

      (* calculate the best r for an angle *)
let best_r img ang = 
  let (w,h) = get_dims img in
  let histo = Array.make (w+h) 0 and r = ref 0 and pi = 4.0 *. (atan 1.0) in
  let c = cos(ang/.180.*.pi) and s = sin(ang/.180.*.pi) in
    for x = 0 to w-1 do
      for y = 0 to h-1 do
        if (Sdlvideo.get_pixel_color img x y) = (0,0,0) then
          begin
            r := (round ((float_of_int x)*.c+.(float_of_int y)*.s));
            if (!r) >= 0 then
              histo.(!r) <- histo.(!r) + 1
          end
      done
    done;
    let somme = ref 0 in
      for k = 0 to w+h-2 do
        somme := !somme + (histo.(k)-histo.(k+1)) * (histo.(k)-histo.(k+1))
      done;
      somme := !somme + histo.(w+h-1)
      (*!somme*)


(* determine the best angle between two values with hough method *)

let best_ang img ang1 ang2 =
  let (w,h) = get_dims img in
  let histo1 = Array.make (w+h) 0 and histo2 = Array.make (w+h) 0 and r = ref 0 and pi = 4.0 *. (atan 1.0) in
  let c1 = cos(ang1/.180.*.pi) and s1 = sin(ang1/.180.*.pi) and c2 = cos(ang2/.180.*.pi) and s2 = sin(ang2/.180.*.pi) in
    for x = 0 to w-1 do
      for y = 0 to h-1 do
        if (Sdlvideo.get_pixel_color img x y) = (0,0,0) then
          begin
            r := (round ((float_of_int x)*.c1+.(float_of_int y)*.s1));
            if (!r) >=0 then
              histo1.(!r) <- histo1.(!r) + 1;
            r := (round ((float_of_int x)*.c2+.(float_of_int y)*.s2));
            if (!r) >= 0 then
              histo2.(!r) <- histo2.(!r) + 1
          end
      done
    done;
    
    let somme1 = ref 0 and somme2 = ref 0 in
      for k = 0 to w+h-2 do
        somme1 := !somme1 + (histo1.(k)-histo1.(k+1)) * (histo1.(k)-histo1.(k+1));
        somme2 := !somme2 + (histo2.(k)-histo2.(k+1)) * (histo2.(k)-histo2.(k+1))
      done;
      
      if (!somme1) > (!somme2) then ang1
      else ang2;;

(* Find the rotation angle with hough method and dichotomy *)

let apply_hough_dich img  =
  (*let (w,h) = get_dims img in*)
  let rec dicho debut fin = function
    | 0 -> best_ang img debut fin
    | a as n ->
      begin
        
        if (best_ang img debut fin) = debut then
          if debut <= 0. && fin <= 0. then
            dicho debut (debut-.(debut-.fin)/.2.) (n-1)
          else if debut >= 0. && fin >= 0. then
              dicho debut (debut+.(fin-.debut)/.2.) (n-1)
            else dicho debut (debut+.(fin-.debut)/.2.) (n-1)
        else 
          if debut <= 0. && fin <= 0. then
            dicho (debut-.(debut-.fin)/.2.) fin (n-1)
          else if debut >= 0. && fin >= 0. then
              dicho (debut+.(fin-.debut)/.2.) fin (n-1)
            else dicho (debut+.(fin-.debut)/.2.) fin (n-1)
      end
  in dicho (-15.) (15.) 15;;

(*let final img =
  let angle_int = (hough img) - 90 in
  let angle = float_of_int angle_int  in
  apply_rot img angle;
  best_quality img;
img;;*)

let final img = 
  let angle = apply_hough_dich img in
  best_r img angle;
  apply_rot img angle;;




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
    (*image2gray img;*)
    show img display;
    wait_key ();
      (* on affiche l'image *)
      show (final img) display;
      (* on attend une touche *)
      wait_key ();
     (* on quitte *)
      exit 0
  end
 
let _ = main ()
