(* Module Iostream *)
(* using : Stdlib *)

class file name =
	object (this)
		val file:string = name

		method add_string s = 
			let b = this#read_string in
			this#print_string (b ^ s)
		method read_lines = 
			let ret = ref (ref []) in
			ignore (this#foreach_line (function l -> !ret := (ref l)::!(!ret)));
			!ret := List.rev !(!ret);
			Stdlib.string_list_to_array !ret
		method read_line i = 
			let i = ref i and ret = ref "" in
			let f l = 
				(if !i == 1 then
					ret := l);
				i := !i - 1
			in if this#foreach_line f = 0 then !ret else ""
		method lines_count = 
			let i = ref 0 in
			if this#foreach_line (fun _ -> i := !i + 1) = 0 then 
			!i else -1
		method print_string s =
			this#forstream_out (function f -> Printf.fprintf f "%s" s)
		method read_string =
			let s = ref (ref "") in
			ignore (this#foreach_line (function l -> !s := !(!s) ^ (if !(!s) <> "" then "\n" else "") ^ l ));
			!(!s)
		method foreach_line f = 
			let parc = function stream ->
			try
				while true do
				f (input_line stream)
				done
			with End_of_file -> () in
			this#forstream_in parc
		method forstream_out f = 
			try
			let stream = open_out file in
			ignore (f stream);
			close_out stream;
			0
			with e -> 1
		method forstream_in f =
			try
			let stream = open_in file in
			ignore (f stream);
			close_in stream;
			0
			with e-> 1
	end

