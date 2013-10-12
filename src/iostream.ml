(* Module Iostream *)

class file name =
	object (this)
		val file:string = name

		method writestring s =
			try 
				let stream = open_out name in
				Printf.fprintf stream "\n%s\n" s;
				close_out stream;
				0
			with e -> 1
		method readlines = 
			let ret = ref [] in
			(try 
				let stream = open_in name in
				try 
				while true do
					ret := (input_line stream)::(!ret)
				done
				with End_of_file ->
					(close_in stream;
					ignore (List.rev (!ret)));
				()
			with e -> () ) ;
			ret
	end
