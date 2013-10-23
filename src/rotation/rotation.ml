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
  let (w,h) = Sdltools.get_img_dim img in
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
  let (w,h) = Sdltools.get_img_dim  img in
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
  let (w,h) = Sdltools.get_img_dim img in
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
  in dicho (-15.) (15.) 15


let final img = 
  let angle = apply_hough_dich img in
  best_r img angle;
  Printf.printf "Rotation angle : %f\n" angle;
  apply_rot img angle

let rotation input output = 
    let img = Sdltools.load_image input in 
    Sdltools.save output (final img)

let rotsec input output angle =
    let img = Sdltools.load_image input in
    Sdltools.save output (apply_rot img angle)

let _ =
    if Array.length (Sys.argv) == 3 then
        rotation (Sys.argv.(1)) (Sys.argv.(2)) 
    else
        Printf.printf "%s inputfile outputfile - Reads inputfile, applies the
        rotation and saves in the outputfile.\n" (Sys.argv.(0))
