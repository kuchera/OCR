let _ = 
	(*
	if Array.length (Sys.argv) == 4 && Sys.argv.(1) = "draw" then
		ignore (Detect.detect ~draw:true (Sys.argv.(2)) (Sys.argv.(3)))
	else if Array.length (Sys.argv) == 3 then
		ignore (Detect.detect (Sys.argv.(1)) (Sys.argv.(2)))
	else
		Printf.printf "%s draw IMAGE_NAME OUTPUT_FILE - Draws the histogram and saves the result.\n%s IMAGE_NAME OUTPUT_FILE - Detects characters and saves the result as : x1;y1;w1;h2|x2;y2...\n" (Sys.argv.(0)) (Sys.argv.(0))
	*)
	if Array.length Sys.argv == 3 then 
		Detect.detectChars Sys.argv.(1) Sys.argv.(2)
	else if Array.length Sys.argv == 2 then
		Detect.detectChars Sys.argv.(1) "files/network/detected/"
	else
		Printf.printf "%s input_file_image output_path - writes each character of the input file to separate files in output_path.\n%s input_filename - the same with path files/network/detected/\n" Sys.argv.(0) Sys.argv.(0)
