let _ = 
	if Array.length (Sys.argv) >= 3 then
		if Sys.argv.(1) = "draw" then
			Detect.draw (Sys.argv.(2))
		else
			ignore (Detect.detect (Sys.argv.(1)) (Sys.argv.(2)))
	else
		Printf.printf "%s draw IMAGE_NAME - Draws the histogram.\n%s IMAGE_NAME OUTPUT_FILE - Detects characters and saves the result as : x1;y1;w1;h2|x2;y2...\n" (Sys.argv.(0)) (Sys.argv.(0))
