let _ = 
	let a = [| [|4;2;42|] ; [|42;42;42;42;42|] |] in
	let s = Stdlib.intmat_to_string a '|' ';' in
	let a = Stdlib.string_to_intmat s '|' ';' in 
	let s = Stdlib.intmat_to_string a '|' ';' in
	Printf.printf "%s\n" s

	(*let screen = Sdltools.start 400 200 in
	let font = Sdlttf.open_font "files/arial.ttf" 30 in
	ignore (exit 0);
	let txt = Sdlttf.render_utf8_blended font "Test <3" ~fg:(Sdlvideo.black)in
	let imgb = Sdlvideo.create_RGB_surface_format screen [] 100 100 in
	Sdlvideo.fill_rect imgb (Int32.of_int 0xFFFFFF);
	Sdltools.show_img imgb screen;
	Sdltools.show_img txt screen;
	Sdltoo.wait_key () *)

(*
mples charset fonts size col row =
    begin
        let res = Array.init (Array.length charset * Array.length fonts) (fun _ -> (Array.make  0 0., -1)) in
        let count = ref 0 in
        for i = 0 to Array.length fonts -1 do (* for each font *)
            Printf.printf "Loading %s...\n" fonts.(i);
            let myfont = Sdlttf.open_font fonts.(i) size in
            for j = 0 to Array.length charset -1 do (* for each letter *)
                let tmp = Sdlttf.render_utf8_blended myfont charset.(j) ~fg:Sdlvideo.black in (*texte*)
                let tw,th = Sdlttf.size_text myfont charset.(j) in
                let pic = Sdlvideo.create_RGB_surface_format tmp [] (tw+1) (th+1) in
                Sdlvideo.fill_rect pic (Int32.of_int 0xFFFFFF);
                Sdlvideo.blit_surface ~src:tmp ~dst:pic ~dst_rect:(Sdlvideo.rect 0 0 0 0) ();
     
    (*
                let w,h = Tools.get_dim (pic) in
                let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
                Tools.print_image (pic) surface;
                (*wait_key ();*)
    *)
                res.(!count) <- (Network.normalize (pic) col row, j);
                count := !count +1;
            done;
        done;
        res;
    end
*)
