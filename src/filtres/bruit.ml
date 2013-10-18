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


let get_arround matrix x y = 
        let l = ref [] in 
        for i = x-1 to x+1 do
            for j = y-1 to y+1 do
                try l := (matrix.(i).(j)) :: (!l) ; () with e -> ()
            done
        done;
        tri (!l)


let rec list_ieme l i = match l with 
    | e::l -> if i <= 0 then e else list_ieme l (i-1)
    | [] -> failwith "Depassement de liste."

let median l = list_ieme l (((List.length l) / 2) - 2)

let erase matrix img surface w h = 
	for i = 0 to w-2 do 
		for j = 0 to h-2 do
	let l = get_arround matrix i j in
	let med = median l in   
	Sdlvideo.put_pixel_color surface i j med
		done
	done

let save img = Sdlvideo.save_BMP img "sans_bruit.bmp"

