val get_img_dim : Sdlvideo.surface -> int * int
val wait_key : unit -> unit
val wait_event : unit -> Sdlevent.event
val show_img : Sdlvideo.surface -> Sdlvideo.surface -> unit
val load_image : string -> Sdlvideo.surface
val init_display : int -> int -> Sdlvideo.surface