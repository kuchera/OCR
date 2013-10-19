val string_of_char : char -> string
val string_list_to_array : string ref list ref -> string ref array
val array_from_refarray : 'a ref array -> 'a array
val split_string : string -> char -> string ref array
val string_of_int4 : int * int * int * int -> string -> string
val int4_of_string : string -> string -> int * int * int * int
val remove_char_from : string -> char -> string
val remove_string_from : string -> string -> string
val stringlist_to_string : string list -> char -> string
val stringarray_to_string : string array -> char -> string
val floatarray_to_string : float array -> char -> string
val doublefloatarray_to_string : float array array -> char -> char -> string
val string_to_floatarray : string -> char -> float array
val string_to_doublefloatarray : string -> char -> char -> float array array
