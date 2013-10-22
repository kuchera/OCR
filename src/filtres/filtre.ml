let print_help () =
		Printf.printf "%s INPUT_IMAGE OUTPUT_IMAGE - Takes away noise and binarises the image.\n%s noise INPUT_FILENAME OUTPUT_FILENAME - Takes away noise.\n%s binarize INPUT_FILENAME OUTPUT_FILENAME - Binarises the image.\n%s niblack INPUT_FILENAME OUTPUT_FILENAME - Applies the Niblack algorithm.\n" (Sys.argv.(0)) (Sys.argv.(0)) (Sys.argv.(0)) (Sys.argv.(0))
	
let _ = 
	if Array.length (Sys.argv) >= 4 then
		if Sys.argv.(1) = "noise" then
			Bruit.remove_noise (Sys.argv.(2)) (Sys.argv.(3))
		else if Sys.argv.(1) = "binarize" then
			Binarisation.binarize (Sys.argv.(2)) (Sys.argv.(3))
                else if Sys.argv.(1) = "niblack" then
                        Niblack.binarize (Sys.argv.(2)) (Sys.argv.(3))
		else
			print_help ()
	else if Array.length (Sys.argv) >= 3 then
		(Bruit.remove_noise (Sys.argv.(1)) (Sys.argv.(2));
		Binarisation.binarize (Sys.argv.(1)) (Sys.argv.(2)))
	else
		print_help ()
