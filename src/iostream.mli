class file :
  string ->
  object
    val file : string
    method add_string : string -> int
    method foreach_line : (string -> unit) -> int
    method forstream_in : (in_channel -> unit) -> int
    method forstream_out : (out_channel -> unit) -> int
    method lines_count : int
    method print_string : string -> int
    method read_line : int -> string
    method read_lines : string ref array
    method read_string : string
  end
val read_file : string -> string
val write_file : string -> string -> int
