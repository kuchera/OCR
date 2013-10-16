val get_dims : Sdlvideo.surface -> int * int
val sdl_init : unit -> unit
val wait_key : unit -> unit
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit
val level : int * int * int -> float
val color2grey : int * int * int -> int * int * int
val image2gray : Sdlvideo.surface -> unit
val matrix : int -> int -> 'a -> 'a array array
val round : float -> int
val apply_rot : Sdlvideo.surface -> float -> Sdlvideo.surface
val best_r : Sdlvideo.surface -> float -> unit
val best_ang : Sdlvideo.surface -> float -> float -> float
val apply_hough_dich : Sdlvideo.surface -> float
val final : Sdlvideo.surface -> Sdlvideo.surface
val main : unit -> 'a
