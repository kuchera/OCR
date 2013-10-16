val get_dims : Sdlvideo.surface -> int * int
val sdl_init : unit -> unit
val wait_key : unit -> unit
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit
val level : int * int * int -> float
val color2grey : int * int * int -> int * int * int
val matrix : int -> int -> (int * int * int) array array
val seuil : Sdlvideo.surface -> int -> int -> int ref
val binary : Sdlvideo.surface -> Sdlvideo.surface
val save : Sdlvideo.surface -> unit
val main : unit -> 'a
