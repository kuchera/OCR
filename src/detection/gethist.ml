(* *** BASIC FUNCTIONS *** *)
let get_dims img =
        ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let reverse list1 =   
	let rec reverser rl = function 
		| a::l1 -> reverser (a::rl) l1
		| _ -> rl
	in reverser [] list1

(* let copyImg img =
        let (w, h) = get_dims img in
	let img2 = (Sdlvideo.create_RGB_surface_format img [] w h) in
        for i = 0 to w-1 do
                for j = 0 to h-1 do
                         let c = Sdlvideo.get_pixel_color img i j in
                                Sdlvideo.put_pixel_color img2 i j c
                done
        done;
	img2 *)


(* *** UTILITY FUNCTIONS *** *)
let is_black (r, g, b) = match (r, g, b) with
	|( 0, 0, 0) -> true
	|_ -> false

let getYHist img =
	let (w, h) = get_dims img in
	let histogramme = Array.init h (fun _ -> 0) in
	for j = 0 to h-1 do
		for i = 0 to w-1 do 
			let c = Sdlvideo.get_pixel_color img i j in
				if (is_black c) then
					histogramme.(j) <- histogramme.(j) + 1
		done
	done;
	histogramme

let getXHist img (bs, bi) =
        let (w, h) = get_dims img in
        let histogramme = Array.init w (fun _ -> 0) in
        for i = 0 to w-1 do
                for j = bs to bi do 
                        let c = Sdlvideo.get_pixel_color img i j in
                                if (is_black c) then
                                        histogramme.(i) <- histogramme.(i) + 1
                done
        done;
        histogramme

let getLines ?(prst=0) ?(misc = false) img new_img = 
	let (w, h) = get_dims img in 
	let hist = getYHist img in
	let inLine = ref false in
	let str = ref 0 in
	let l = ref [] in
	for j = 0 to h-1 do
		match hist.(j) with
			| n when (not !inLine) && (n > prst) -> inLine := true;
			  str := j;
			| n when (!inLine) && (n < prst + 1) -> inLine := false;
			  if (misc) then
				let rec auxFunction = match ((j - !str) / 4) with
					|aux when (j+aux) >= h && (!str - aux) >= 0 -> l := (!str - aux, j)::!l
					|aux when (j+aux) <= h && (!str - aux) <= 0 -> l := (!str, j + aux)::!l
					|aux when (j+aux) <= h && (!str - aux) >= 0 -> l := (!str - aux, j + aux)::!l
					|_ -> l := (!str, j)::!l
				in auxFunction
			  else
				l := (!str, j)::!l;
			|_ -> ()
	done;
	(*Epuration des intervales bugués *)
	let rec epur l1 l2 = match l1 with
		|(bs, bi)::l when (bi -bs) < 5 -> epur l l2
		|(bs, bi)::l -> epur l ((bs, bi)::l2)
		|[] -> l2
	in l := epur !l [];
	(*On dessine les lignes sur l'image *) 	
	let rec drawLines liste = match liste with
		|(bs, bi)::l -> 
		for i = 0 to w - 1 do
			Sdlvideo.put_pixel_color new_img i bs (0, 0, 0);
			Sdlvideo.put_pixel_color new_img i bi (0, 0, 0);
		done;
		drawLines l;
		|_ -> ()
	in drawLines !l;
	reverse !l (* on veut les lignes du haut vers le bas et pas l'inverse *)

let getChars ?(prst=0) img new_img linesList =
	let (w, h) = get_dims img in
	let inColumn = ref false in
	let str = ref 0 in
	let rectList = ref [] in
	let rec getOneChar liste = match liste with
		|(bs, bi)::l ->
		 let hist = getXHist img (bs, bi) in 
		 for i = 0 to w-1 do
			match hist.(i) with
				| n when (not !inColumn) && (n > prst) ->
				inColumn := true;
				str := i;
				| n when (!inColumn) && (n < prst + 1) ->
				inColumn := false;
				rectList := ((bs, !str - 1),(bi, i + 1))::!rectList;
				|_ -> () 
		 done;
		 getOneChar l;
		|_ -> ()
	in getOneChar linesList;
	(*Epuration des intervales bugués *)
        let rec epur l1 l2 = match l1 with
                |((bs, i1),(bi, i2))::l when (i2 -i1) < 4 -> epur l l2
                |((bs, i1),(bi, i2))::l -> epur l (((bs, i1),(bi, i2))::l2)
                |_ -> l2
        in rectList := epur !rectList [];

	let rec drawColumns liste = match liste with
		|((bs, x1),(bi, x2))::l ->
                 for j = bs to bi do
                        Sdlvideo.put_pixel_color new_img x1 j (0, 0, 0);
                        Sdlvideo.put_pixel_color new_img x2 j (0, 0, 0);
                 done;
		 drawColumns l;
		|_ -> ()
	in drawColumns !rectList;
	reverse !rectList (* on veut les characteres de gauche à droite *)
 
			

(* *** TESTS FUNCTIONS *** *)
let drawHist img new_img =
	let (w, h) = get_dims img in 
	let hist1 = getYHist img in
	let hist2 = getXHist img (0, h-1) in
	for j = 0 to h-1 do
		for i = 0 to (hist1.(j) / 10) do
                	Sdlvideo.put_pixel_color new_img i j (30, 127, 230)
		done
	done;
	for i = 0 to w-1 do
        	for j = 0 to (hist2.(i) / 5) do
                        Sdlvideo.put_pixel_color new_img i j (209, 182, 6)
                done
        done


let main () =
	begin
		if Array.length (Sys.argv) < 2 then
			failwith "Trop peu d'arguments";
		sdl_init ();
		let img = Sdlloader.load_image Sys.argv.(1) in
		let (w, h) = get_dims img in
		let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
		show img display;
		wait_key();
		let new_img = Sdlloader.load_image Sys.argv.(1) in
		(* let new_img = copyImg img in *)
		drawHist img new_img;
		let l = getLines ~prst:3 ~misc:false img new_img in
		getChars ~prst:1 img new_img l;
		show new_img display;
		wait_key();
		exit 0
	end

let _ = main ()	