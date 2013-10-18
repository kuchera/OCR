let greyscale (r,g,b) =
    ( 0.3 *. (float_of_int r) +. 0.59 *. (float_of_int g) +. 0.11 *.
    (float_of_int b))

let gethist img = 
    let (w, h) = Sdltools.get_img_dim img and
    hist = Array.init 257 (fun _  -> 0 ) in 
    for i = 0 to w-1 do
        for j = 0 to h-1 do
            let greylvl = greyscale (Sdlvideo.get_pixel_color img i j) in 
                hist.(int_of_float greylvl) <- hist.(int_of_float
                greylvl) + 1
        done
    done;
    hist

let rec power x n = match n with
    | 0 -> 1.
    | n -> x *. ( power x (n-1) )

let binarise img img2 = 
    let (w, h) = Sdltools.get_img_dim img in
    let nbp = (float_of_int w *. float_of_int h) and
    moy = Array.init 256 (fun _ -> 0.) and
    ect = Array.init 256 (fun _ -> 0.) and
    hist = gethist img in

    (* moyenne et ecart type *)
    let prp = Array.init 256 (fun _ -> 0.) in
    for y = 0 to 256 do
        prp.(y) <- float_of_int (hist.(y)) /. nbp;
    done;
    ect.(0) <- prp.(0);
    for z = 0 to 256 do 
        ect.(z) <- ect.(z-1) +. prp.(z);
        moy.(z) <- moy.(z-1) +. ((float_of_int (z + 1 )) *. prp.(z));
    done;
    
    (* Sigma maximisation Interclass variance *)
    
    let threshold = ref 0 and
    sigma = Array.init 256 (fun _ -> 0.) and
    max_sigma = ref 0. in
    for x = 1 to 255 do
        if (ect.(x) = 0. || ect.(x) = n) then
            sigma.(x) <- (power (moy.(255) *. ect.(x) -. moy.(x)) 2) /. 
            (ect.(x) *. (1. -. ect.(x) ))
        else
            ( sigma.(x) <- 0. );
        else
            let aux = moy.(255) *. ect.(x) -. nbp *. moy.(x) in
                sigma := (aux *. aux) /. (nbp *. nbp *. ect.(x) )
                *. ( nbp -. ect.(x));
        if ( sigma.(x)  > !max_sigma ) then
            begin
                max_sigma := sigma.(x);
                threshold := x;
            end
    done;
    for i = 0 to w-1 do
        for j = 0 to h-1 do
            if (int_of_float (greyscale(Sdlvideo.get_pixel_color img i j)) < !threshold) then
                Sdlvideo.put_pixel_color img2 i j (0, 0, 0)
            else
                Sdlvideo.put_pixel_color img2 i j (255, 255, 255);
        done
    done
    
let main () =
    begin
        if Array.length (Sys.argv) < 2 then
            failwith "Trop peu d'arguments";                    
        let img = Sdlloader.load_image Sys.argv.(1) in
        let new_img = Sdlloader.load_image Sys.argv.(1) in
        let (w, h) = Sdltools.get_img_dim img in
        let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
        Sdltools.show_img img display;
        Sdltools.wait_key();
        binarise img new_img;
        Sdltools.show_img new_img display;
        Sdltools.wait_key();
        exit 0
    end


let _ = main ()
