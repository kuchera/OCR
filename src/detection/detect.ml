let reverse list1 =   
	let rec reverser rl = function 
		| a::l1 -> reverser (a::rl) l1
		| _ -> rl
	in reverser [] list1

let is_black (r, g, b) = match (r, g, b) with
	|( 0, 0, 0) -> true
	|_ -> false

let getYHist img =
	let (w, h) = Sdltools.get_img_dim img in
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
        let (w, h) = Sdltools.get_img_dim img in
        let histogramme = Array.init w (fun _ -> 0) in
        for i = 0 to w-1 do
                for j = bs to bi do 
                        let c = Sdlvideo.get_pixel_color img i j in
                                if (is_black c) then
                                        histogramme.(i) <- histogramme.(i) + 1
                done
        done;
        histogramme

let getLines ?(prst=0) ?(misc = false) img = 
	let (w, h) = Sdltools.get_img_dim img in 
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
	reverse !l (* on veut les lignes du haut vers le bas et pas l'inverse *)

let getChars ?(prst=0) img linesList =
	let (w, h) = Sdltools.get_img_dim img in
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
	(reverse !rectList)

(* DRAW FUNCTIONS *)
let drawLines new_img linesList =
	let (w, h) = Sdltools.get_img_dim new_img in 
        let rec drawLines liste = match liste with
                |(bs, bi)::l ->
                for i = 0 to w - 1 do
                        Sdlvideo.put_pixel_color new_img i bs (0, 0, 0);
                        Sdlvideo.put_pixel_color new_img i bi (0, 0, 0);
                done;
                drawLines l;
                |_ -> ()
        in drawLines linesList


let drawChars new_img charsList = 
	let rec drawColumns liste = match liste with
		|((bs, x1),(bi, x2))::l ->
                 for j = bs to bi do
                        Sdlvideo.put_pixel_color new_img x1 j (0, 0, 0);
                        Sdlvideo.put_pixel_color new_img x2 j (0, 0, 0);
                 done;
		 drawColumns l;
		|_ -> ()
	in drawColumns charsList
			

let drawHist img new_img =
	let (w, h) = Sdltools.get_img_dim img in 
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

let detectText ?(mustDraw=false) imgPath =
        Sdltools.sdl_init ();
        let img = Sdlloader.load_image imgPath in
	let linesList = getLines ~prst:3 ~misc:true img in
	let charsList = getChars ~prst:1 img linesList in
	if( mustDraw = true ) then
	begin
        	let (w, h) = Sdltools.get_img_dim img in
               	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
		let new_img = Sdlloader.load_image imgPath in
		drawHist img new_img;
		drawChars new_img charsList;
		drawLines new_img linesList;
		Sdltools.show_img new_img display;
		Sdltools.wait_key();
	end;
	let rec convertList liste1 liste2 = match liste1 with
	|((bs, x1),(bi, x2))::l -> let str = (string_of_int bs)^" "^(string_of_int x1)^" "^(string_of_int (bi -bs))^" "^(string_of_int (x2 -x1)) in
	  convertList l (str::liste2)
	|[] -> liste2
	in convertList charsList []


let _ = 
	if Array.length (Sys.argv) < 3 then
                failwith "(Param. 1 : Path to Picture | Param. 2 : Path to Text file";
	let l = detectText ~mustDraw:false Sys.argv.(1) in
		let rec writeFile liste = match liste with  
			| [] -> exit 0
			| a::liste -> ignore (Iostream.write_file (Sys.argv.(2)) (a^"|"));
			writeFile liste
		in writeFile l

