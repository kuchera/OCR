(* Module Sdltools *)

let get_img_dim img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
(* 
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 *)
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
