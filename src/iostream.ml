(* Module Iostream *)
(* using : Stdlib *)

class file name =
	object (this)
		val file:string = name

		method print_string s =
			try 
				let stream = open_out name in
				Printf.fprintf stream "%s" s;
				close_out stream;
				0
			with e -> 1
		method read_lines = 
			let ret = ref [] in
			(try 
				let stream = open_in name in
				(try 
				while true do
					ret := (ref (input_line stream))::(!ret)
				done
				with End_of_file -> close_in stream);
				ret := List.rev (!ret);
				()
			with e -> () ) ;
			Stdlib.string_list_to_array ret
		method read_line i = 
			try 
				let ret = ref "" and stream = open_in name and i = ref i in
				try 
					while !i > 0 do
						ret := input_line stream;
						i := !i - 1
					done;
					!ret
				with End_of_file -> close_in stream; ""
			with e -> ""
		method lines_count = 
			try
				let i = ref 0 and stream = open_in name in
				try
				while true do
					ignore (input_line stream);
					i := !i + 1
				done; !i
				with End_of_file -> close_in stream; !i
			with e -> 0
	end
