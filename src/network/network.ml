let dirdetect = "files/network/"

let show i =
	let s = Iostream.read_file (dirdetect ^ "detected/"^(string_of_int i)^".mat") in
	let a = Stdlib.string_to_intmat s '|' ';' in
	Array.iter (fun e ->
		Array.iter (fun ee -> Printf.printf "%d " ee) e;
		Printf.printf "\n") a


let f =
	Learn.learn ()
