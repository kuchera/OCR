val get_dims : Sdlvideo.surface -> int * int
val sdl_init : unit -> unit
val wait_key : unit -> unit
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit
val img_to_matrix : Sdlvideo.surface -> (int * int * int) array array
val insert :
  int * int * int -> (int * int * int) list -> (int * int * int) list
val tri : (int * int * int) list -> (int * int * int) list
val eight_pixels :
  (int * int * int) array array -> int -> int -> (int * int * int) list
val list_ieme : 'a list -> int -> 'a
val median : 'a list -> 'a
val erase :
  (int * int * int) array array ->
  'a -> Sdlvideo.surface -> int -> int -> unit
val save : Sdlvideo.surface -> unit
val main : unit -> 'a
