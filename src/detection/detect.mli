val reverse : 'a list -> 'a list
val is_black : int * int * int -> bool
val getYHist : Sdlvideo.surface -> int * int * int * int -> int array
val getXHist : Sdlvideo.surface -> int * int -> int array
val getLines :
  ?prst:int ->
  ?misc:bool -> Sdlvideo.surface -> int * int * int * int -> (int * int) list
val getChars :
  ?prst:int ->
  Sdlvideo.surface -> (int * int) list -> ((int * int) * (int * int)) list
val getBlocColumns :
  ?prst:int ->
  Sdlvideo.surface -> (int * int) list -> ((int * int) * (int * int)) list
val getBlocs :
  ?prst:int -> Sdlvideo.surface -> ((int * int) * (int * int)) list
val drawBlocs : Sdlvideo.surface -> ((int * int) * (int * int)) list -> unit
val drawLines : Sdlvideo.surface -> (int * int) list -> unit
val drawChars : Sdlvideo.surface -> ((int * int) * (int * int)) list -> unit
val drawHist : Sdlvideo.surface -> Sdlvideo.surface -> unit
val detectText : ?mustDraw:bool -> string -> ((int * int) * (int * int)) list
val get_int4_array : ?draw:bool -> string -> (int * int * int * int) array
val detect : ?draw:bool -> string -> string -> int
val detectChars : string -> string -> unit
