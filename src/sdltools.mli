val set_window_name : string -> string -> unit
val get_img_dim : Sdlvideo.surface -> int * int
val sdl_init : unit -> unit
val wait_key : unit -> unit
val wait_event : unit -> Sdlevent.event
val show_img : Sdlvideo.surface -> Sdlvideo.surface -> unit
val load_image : string -> Sdlvideo.surface
val init_display : int -> int -> Sdlvideo.surface
val save : string -> Sdlvideo.surface -> unit
val create_surface : Sdlvideo.surface -> int -> int -> Sdlvideo.surface
val create_colored_surface :
  Sdlvideo.surface ->
  int -> int -> int -> int -> int -> 'a -> Sdlvideo.surface
val start : int -> int -> Sdlvideo.surface
