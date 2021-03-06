(* Module Sdltools *)

let set_window_name title icon = Sdlwm.set_caption title icon

let get_img_dim img =
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

let wait_event () = Sdlevent.wait_event ()
 
let show_img img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let load_image name = Sdlloader.load_image name

let init_display w h = Sdlvideo.set_video_mode w h [`DOUBLEBUF]

let save filename img = Sdlvideo.save_BMP img filename

let create_surface screen w h = Sdlvideo.create_RGB_surface [`HWSURFACE] w h (Sdlvideo.surface_bpp screen) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255)

let create_colored_surface screen w h r g b a =
	let s = Sdlvideo.create_RGB_surface_format screen [] w h in
	Sdlvideo.fill_rect s (Sdlvideo.map_RGB screen (r,g,b));
	s

let start w h = sdl_init (); init_display w h
