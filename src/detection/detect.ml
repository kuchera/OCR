let reverse list1 =   
	let rec reverser rl = function 
		| a::l1 -> reverser (a::rl) l1
		| _ -> rl
	in reverser [] list1

let is_black (r, g, b) = match (r, g, b) with
	|( 0, 0, 0) -> true
	|_ -> false

let getYHist img (xinf, xsup, yinf, ysup) =
	let (w, h) = Sdltools.get_img_dim img in
	let histogramme = Array.init h (fun _ -> 0) in
	for j = yinf to ysup do
		for i = xinf to xsup do 
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

let getLines ?(prst=0) ?(misc = false) img (xinf, xsup, yinf, ysup) = 
	let (w, h) = Sdltools.get_img_dim img in 
	let hist = getYHist img (xinf, xsup, yinf, ysup) in
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
	!l

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
	!rectList

let getBlocColumns ?(prst=0) img blocsListY =
        let (w, h) = Sdltools.get_img_dim img in
        let inColumn = ref false in
        let str = ref 0 in
        let rectList = ref [] in
        let rec getOneBloc liste = match liste with
                |(bhigh, blow)::l ->
                 let hist = getXHist img (bhigh, blow) in
                 for i = 0 to w-1 do
                        match hist.(i) with
                                | n when (not !inColumn) && (n > prst) ->
                                inColumn := true;
                                str := i;
                                | n when (!inColumn) && (n < prst + 1) ->
                                inColumn := false;
                                rectList := ((bhigh, !str - 1),(blow, i + 1))::!rectList;
                                |_ -> ()
                 done;
                 getOneBloc l;
                |_ -> ()
        in getOneBloc blocsListY;
        (*Epuration des intervales bugués *)
        let rec epur l1 l2 = match l1 with
                |((bs, i1),(bi, i2))::l when (i2 -i1) < 4 -> epur l l2
                |((bs, i1),(bi, i2))::l -> epur l (((bs, i1),(bi, i2))::l2)
                |_ -> l2
        in rectList := epur !rectList [];
        !rectList

let getBlocs ?(prst=0) img =
        let (w, h) = Sdltools.get_img_dim img in
        let intValues = ref [] in
        let lineslist = getLines ~prst:prst ~misc:false img (0, (w-1), 0, (h-1)) in
        let rec aux1 liste b = match (liste, b) with 
		|((bhigh, blow)::l, 0) -> aux1 l blow;
		|((bhigh, blow)::l, b) when b <> 0 -> intValues := (bhigh - b)::(!intValues);
                aux1 liste 0
		|_ -> ()
	in aux1 lineslist 0;
	let mIntV = ref 0 in
	let rec aux2 liste = match liste with
		|v::l -> mIntV := (!mIntV) + v;
			aux2 l
		|_ -> mIntV := !mIntV / List.length lineslist
	in aux2 !intValues;
	let endBlocList = ref [] in
	let rec aux3 liste b = match (liste, b) with
                |((bhigh, blow)::l, 0) -> aux3 l blow
                |((bhigh, blow)::l, b) when b <> 0 -> 
			if ((bhigh - b) > !mIntV) then (endBlocList := ((b + ((bhigh - b) /2))::(!endBlocList)));
			aux3 liste 0
		|_ -> ()
	in aux3 lineslist 0;
	let listBlocsY = ref [] in
	let rec fillYList liste b = match (liste, b) with
		|(e::l, b) -> listBlocsY := (e, b)::(!listBlocsY); fillYList l e
		|_ -> listBlocsY := (0, b)::(!listBlocsY)
	in fillYList !endBlocList (h-1);
	(* Section des blocsY *)
	let columnlist = getBlocColumns ~prst:(prst*2) img !listBlocsY in
	let listBlocs = ref [] in
        let rec getBlocList liste bleft bright b = match (liste, bleft, bright, b) with
                |(((bhigh, bl),(blow, br))::l, brL, brR, b) when brL = -1 -> getBlocList l bl br bhigh
                |(((bhigh, bl),(blow, br))::l, brL, brR, b) when b <> bhigh -> (listBlocs := (((bhigh, brL), (blow, (w-1)))::!listBlocs));
                        getBlocList l (-1) br bhigh
                |(((bhigh, bl),(blow, br))::l, brL, brR, b) when (bl - brR) < 50 -> getBlocList l brL br bhigh
                |(((bhigh, bl),(blow, br))::l, brL, brR, b) -> listBlocs := (((bhigh, brR), (blow, brL))::!listBlocs); getBlocList l bl (-1) bhigh
                |_ -> ()
        in getBlocList columnlist (-1) (-1) (-1);
	(*intValues := [];
	let rec aux4 liste (b, t) = match (liste, (b, t)) with
                |(((bhigh, bl), (blow, br))::l, (0, _)) -> aux4 l (br, bhigh);
                |(((bhigh, bl), (blow, br))::l, (b, lbhi)) when b <> 0 ->
		if lbhi = bhigh then intValues := (bl - b)::(!intValues);
                aux4 liste (0, bhigh)
                |_ -> ()
        in aux4 columnlist (0, 0);
	mIntV := 0;
        let rec aux5 liste = match liste with
                |v::l -> mIntV := (!mIntV) + v;
                        aux5 l
                |_ -> mIntV := !mIntV / List.length columnlist
        in aux5 !intValues;
	print_int !mIntV; *)
	(*mIntV := 25;
	let rec debug liste = match liste with
		|((bhigh, bl),(blow, br))::l -> 
		 Printf.printf "\n BORNE SUP :";
                 print_int bhigh;
                 Printf.printf "\n Borne INF :";
                 print_int blow;
                 Printf.printf "\n Borne gauche:";
                 print_int bl;
                 Printf.printf "\n Borne droite:";
                 print_int br;
		 debug l
		|_ -> ()
	in debug columnlist;
	
	let endBlocList2 = ref [] in 
        let rec aux6 liste (b, t) = match (liste, (b, t)) with
                |(((bhigh, bl),(blow, br))::l, (0, _)) -> aux6 l (bl, bhigh) 
                |(((bhigh, bl),(blow, br))::l, (b, lbhi)) when b <> 0 -> 
                        if ((lbhi = bhigh) && (b - br) > !mIntV) then (endBlocList2 := ((bhigh, blow, (br + ((b - br) /2)))::(!endBlocList2)));
                        aux6 liste (0, bhigh)
                |_ -> ()
        in aux6 columnlist (w-1, 0);
	*)
	(*let rec fillList liste b = match (liste, b) with
                |((bhigh, blow, bend)::l, b) -> listBlocs := ((bhigh, b), (blow, bend))::(!listBlocs); fillList l bend
                |_ -> ()
        in fillList !endBlocList2 (h-1);*)
	!listBlocs
	

(* DRAW FUNCTIONS *)
let drawBlocs new_img blocsList =
	let rec drawBlocs liste = match liste with
                |((bs, x1),(bi, x2))::l ->
                 for i = x1 to x2 do
                        Sdlvideo.put_pixel_color new_img i bs (0, 255, 0);
                        Sdlvideo.put_pixel_color new_img i bi (0, 255, 0);
                 done;
		 for j = bs to bi do
                        Sdlvideo.put_pixel_color new_img x1 j (0, 255, 0);
                        Sdlvideo.put_pixel_color new_img x2 j (0, 255, 0);
                 done;
		 (*printf.printf "\n BORNE SUP :";
		 print_int bs;
		 Printf.printf "\n Borne INF :";
		 print_int bi;
		 Printf.printf "\n Borne gauche:";
                 print_int x1;
		 Printf.printf "\n Borne droite:";
		 print_int x2;*)
                 drawBlocs l;
                |_ -> ()
        in drawBlocs blocsList

let drawLines new_img linesList =
	let (w, h) = Sdltools.get_img_dim new_img in 
        let rec drawLines liste = match liste with
                |(bs, bi)::l ->
                for i = 0 to w-1 do
                        Sdlvideo.put_pixel_color new_img i bs (255, 0, 0);
                        Sdlvideo.put_pixel_color new_img i bi (255, 0, 0);
                done;
                drawLines l;
                |_ -> ()
        in drawLines linesList


let drawChars new_img charsList = 
	let rec drawColumns liste = match liste with
		|((bs, x1),(bi, x2))::l ->
                 for j = bs to bi do
                        Sdlvideo.put_pixel_color new_img x1 j (0, 0, 255);
                        Sdlvideo.put_pixel_color new_img x2 j (0, 0, 255);
                 done;
		 drawColumns l;
		|_ -> ()
	in drawColumns charsList
			

let drawHist img new_img =
	let (w, h) = Sdltools.get_img_dim img in 
	let hist1 = getYHist img (0, w-1, 0, h-1) in
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
	let (w, h) = Sdltools.get_img_dim img in
	(* let blocsList = getBlocs ~prst:5 img in *)
	let linesList = getLines ~prst:5 ~misc:true img (0, w-1, 0, h-1) in
	let charsList = getChars ~prst:1 img linesList in
	if( mustDraw = true ) then
	begin
               	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
		let new_img = Sdlloader.load_image imgPath in
		drawHist img new_img;
		drawChars new_img charsList;
		drawLines new_img linesList;
		(*drawLines new_img blocsList;*)
		(*drawBlocs new_img blocsList;*)
		Sdltools.show_img new_img display;
		Sdltools.wait_key();
	end;
	charsList

let get_int4_array ?(draw=false) image =
	let l = ref (detectText ~mustDraw:draw image) in
	Array.init (List.length !l) (function _ -> 
		match !l with
			| ((bs,bl),(bi,br))::f -> l := f ; (bl, bs, (bi-bs), (br-bl))
			| _ -> failwith "Error in : Detect.detect")

let detect ?(draw=false) image outfile =
	let a = get_int4_array ~draw:draw image in
	let sa = Array.init (Array.length a) (function i -> Stdlib.string_of_int4 (a.(i)) ";") in
	Iostream.write_file outfile (Stdlib.stringarray_to_string sa '|')

let detectChars img path =
	let a = get_int4_array ~draw:true img in
	let size = int_of_string (Iostream.read_file (path^"size")) in
	for i = 0 to (((Array.length a) -1)) do
		let filepath = (path^(string_of_int (i+1))^(".mat")) in
		let matrix = ToMatrix.getMatrix size img (a.(i)) in
		let chaine = Stdlib.intmat_to_string matrix '|' ';' in
		(*if (i = 0) then print_string chaine;*)
		ignore (Iostream.write_file filepath chaine)
	done;
	ignore (Iostream.write_file (path^"count") (string_of_int (Array.length a)))
