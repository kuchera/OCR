let reverse list1 =   
	let rec reverser rl = function 
		| a::l1 -> reverser (a::rl) l1
		| _ -> rl
	in reverser [] list1

let triCharList charList =
	let rec sort = function
	| [] -> []
	| x :: l -> insert x (sort l)
	and insert ((ebhi, ebl),(eblo, ebr)) = function
  	| [] -> [((ebhi, ebl),(eblo, ebr))]
  	| ((bhi, bl),(blo, br)) :: l -> 
      	if ebhi < bhi then
		((ebhi, ebl),(eblo, ebr)) :: ((bhi, bl),(blo, br)) :: l
	else
		if (ebhi = bhi) then
			if (ebl < bl) then
				((ebhi, ebl),(eblo, ebr)) :: ((bhi, bl),(blo, br)) :: l
			else
				((bhi, bl),(blo, br)) :: (insert ((ebhi, ebl),(eblo, ebr)) l)
		else
				((bhi, bl),(blo, br)) :: (insert ((ebhi, ebl),(eblo, ebr)) l)
	in sort charList
	
let is_black (r, g, b) = match (r, g, b) with
	|( 0, 0, 0) -> true
	|_ -> false
	
let getYHist img (bl, br, bhi, blo) =
	(*Printf.printf "bl = %d, br = %d, bhi = %d, blo = %d" bl br bhi blo;*)
        let histogramme = Array.init (blo-bhi) (fun _ -> 0) in
        for j = bhi to blo-1 do
                for i = bl to br-1 do
                        let c = Sdlvideo.get_pixel_color img i j in
                                if (is_black c) then
                                        histogramme.(j-bhi) <- histogramme.(j - bhi) + 1
                done;
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

(*
let drawHist img new_img (bhi, blo, bl ,br) =
        let hist1 = getYHist img (bl, br, bhi, blo) in
        let hist2 = getXHist img (bhi, blo) in
        for j = bhi to blo-1 do
                for i = 0 to (hist1.(j) / 10) do
                        Sdlvideo.put_pixel_color new_img i j (30, 127, 230)
                done
        done;
        for i = bl to br-1 do
                for j = 0 to (hist2.(i) / 5) do
                        Sdlvideo.put_pixel_color new_img i j (209, 182, 6)
                done
        done
*)
let isAnImg img (bhi, blo, bl, br) =
        let histogramme = getYHist img (bl, br, bhi, blo) in
        let rec cmpt = ref 0 in
        for j = bhi to blo-1 do
                if histogramme.(j - bhi) < 5 then
                        cmpt := !cmpt +1
        done;
        (!cmpt < ((blo -bhi) / 10))

let getLines ?(prst=0) ?(misc = false) img (bl, br, bhi, blo)= 
	let (w, h) = Sdltools.get_img_dim img in 
	let hist = getYHist img (bl, br, bhi, blo) in
	let inLine = ref false in
	let str = ref 0 in
	let l = ref [] in
	for j = bhi to blo-1 do
		match hist.(j-bhi) with
			| n when (not !inLine) && (n > prst) -> inLine := true;
			  str := j;
			| n when (!inLine) && (n < prst + 1) -> inLine := false;
			  if (misc) then
				let rec auxFunction = match ((j - !str) / 8) with
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
        (*gestion erreur*)
        if (List.length !l = 0) then
            l := (0,(h-1))::[];
	!l

let getChars ?(prst=0) img linesList (bl, br) =
	let inColumn = ref false in
	let str = ref 0 in
	let rectList = ref [] in
	let rec getOneChar liste = match liste with
		|(bs, bi)::l ->
		 let hist = getXHist img (bs, bi) in
		 for i = bl to br-1 do
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
                 if (hist.(0) > prst) then 
                 begin    
                     inColumn := true; 
                     str := 0
                 end;
                 for i = 0 to w-1 do
                        match hist.(i) with
                                | n when (not !inColumn) && (n > prst) ->
                                inColumn := true;
                                str := i;
                                | n when (!inColumn) && (n < prst + 1) ->
                                inColumn := false;
                                rectList := ((bhigh, !str - 1),(blow, i + 5))::!rectList;
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
		|_ -> 
                        if  List.length !intValues <> 0 then
                            mIntV := !mIntV / List.length !intValues
                        else
                            mIntV := 1000
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
	let columnslist = getBlocColumns ~prst:prst img (!listBlocsY) in
	let intValues2 = ref [] in
	let rec aux4 liste ((lbhi, lbl), (lblo, lbro)) = match (liste) with
		|((bhi, bl), (blo, br))::l when ((lbl = -1)||(lbhi <> bhi)) -> aux4 l ((bhi, bl), (blo, br))
		|((bhi, bl), (blo, br))::l -> intValues2 := (bl - lbro)::(!intValues2); aux4 l ((bhi, bl), (blo, br))
		|_ -> ()
	in aux4 columnslist ((-1, -1), (-1, -1));
	let mIntV2 = ref 0 in
        let rec aux5 liste = match liste with
                |v::l -> mIntV2 := (!mIntV2 + v); aux5 l
                |_ -> mIntV2 := (int_of_float (float(!mIntV2)*.1.2))  / List.length !intValues2
        in aux5 !intValues2;
	let listBlocs = ref [] in
	let rec fillFinalList liste ((lbhi, lbl), (lblo, lbro)) blocstart = match liste with
		|((bhi, bl), (blo, br))::[] when (blocstart = (-1, -1)) -> listBlocs := (((bhi, bl), (blo, br))::!listBlocs)
		|((bhi, bl), (blo, br))::[] -> listBlocs := ((blocstart, (blo, br))::!listBlocs)
		|((bhi, bl), (blo, br))::l when (lbl = -1) -> fillFinalList l ((bhi, bl), (blo, br)) (bhi, bl)
		|((bhi, bl), (blo, br))::l when (lbhi <> bhi) -> 
			let f liste = match liste with
				|(_, e)::l when (e = (lblo, lbro)) -> ()
				|_ -> listBlocs := ((blocstart, (lblo, lbro))::!listBlocs)
			in f !listBlocs;
			fillFinalList l ((bhi, bl), (blo, br)) (bhi, bl)
		|((bhi, bl), (blo, br))::l when (bl - lbro) <= !mIntV2 -> fillFinalList l ((bhi, bl), (blo, br)) blocstart
		|((bhi, bl), (blo, br))::l ->  listBlocs := ((blocstart, (lblo, lbro))::!listBlocs); fillFinalList l ((bhi, bl), (blo, br)) (bhi, bl)
		|[] -> ()
	in fillFinalList columnslist ((-1, -1), (-1, -1)) (-1, -1);
	
	(*Epuration des blocs*)
	let rec epur l1 l2 = match l1 with
                |((bhi, bl),(blo, br))::l ->
			if ( isAnImg img (bhi, blo, bl, br) ) then
                		epur l l2
			else
				epur l (((bhi, bl),(blo, br))::l2)
		|_ -> l2
        in epur !listBlocs [] 
	

(* DRAW FUNCTIONS *)
let drawBlocs new_img blocsList =
	let rec drawBlocs liste = match liste with
                |((bs, bl),(bi, br))::l->
                 for i = bl to br-1 do
                        Sdlvideo.put_pixel_color new_img i bs (0, 255, 0);
                        Sdlvideo.put_pixel_color new_img i bi (0, 255, 0);
                 done;
		 for j = bs to bi-1 do
                        Sdlvideo.put_pixel_color new_img bl j (0, 255, 0);
                        Sdlvideo.put_pixel_color new_img br j (0, 255, 0);
                 done;
                 drawBlocs l;
                |_ -> ()
        in drawBlocs blocsList

let drawLines new_img linesList (bl, br)= 
        let rec drawLines liste = match liste with
                |(bs, bi)::l ->
                for i = bl to br-1 do
                        Sdlvideo.put_pixel_color new_img i bs (255, 0, 0);
                        Sdlvideo.put_pixel_color new_img i bi (255, 0, 0);
                done;
                drawLines l;
                |_ -> ()
        in drawLines linesList

(*
let drawChars new_img charsList = 
	let rec drawColumns liste = match liste with
		|((bs, x1),(bi, x2))::l ->
                 for j = bs to bi-1 do
                        Sdlvideo.put_pixel_color new_img x1 j (0, 0, 255);
                        Sdlvideo.put_pixel_color new_img x2 j (0, 0, 255);
                 done;
		 drawColumns l;
		|_ -> ()
	in drawColumns charsList			
*)


let getAllChars blocList img =
	let charsList = ref [] in
	let rec fillList bliste = match bliste with
		|((bhi, bl), (blo, br))::l ->
			let linesList = getLines ~prst:2 ~misc:true img (bl, br, bhi, blo) in
			let blocCharsList = getChars ~prst:0 img linesList (bl, br) in
			charsList := blocCharsList@(!charsList);
			fillList l
		|_ -> ()
	in fillList blocList;
	!charsList
	
let detectText ?(mustDraw=false) imgPath =
        Sdltools.sdl_init ();
        let img = Sdlloader.load_image imgPath in
	let (w, h) = Sdltools.get_img_dim img in
	let blocsList = getBlocs ~prst:2 img in	
	let charsList = getAllChars blocsList img in
	(*let linesList = getLines ~prst:2 ~misc:true img (0, w-1, 0, h-1) in
	let charsList = getChars ~prst:1 img linesList (0, w-1)in*)
	(*Triage de la liste de chars*)
	let sortedCharsList = triCharList charsList in
	if( mustDraw = true ) then
	begin
               	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
		let new_img = Sdlloader.load_image imgPath in
		(*drawHist img new_img;*)
		(*drawChars new_img charsList;*)
		(*drawLines new_img linesList;*)
		(*drawBlocs new_img blocsList;*)
		drawBlocs new_img sortedCharsList;
		Sdltools.show_img new_img display;
                Sdltools.save "files/out.bmp" new_img;
		Sdltools.wait_key();
	end;
	sortedCharsList


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
