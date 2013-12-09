let _ = 
        let n = new Graph.graph 0.5 [|2;2;1|] in
        for i=0 to 100000 do
            n#training [|0.;0.|] [|0.|];
            n#training [|1.;0.|] [|1.|];
            n#training [|0.;1.|] [|1.|];
            n#training [|1.;1.|] [|0.|];
        done;
        Printf.printf "0 0 -> %f\n" (n#get_out [|0.;0.|]).(0);
        Printf.printf "0 1 -> %f\n" (n#get_out [|0.;1.|]).(0);
        Printf.printf "1 0 -> %f\n" (n#get_out [|1.;0.|]).(0);
        Printf.printf "1 1 -> %f\n" (n#get_out [|1.;1.|]).(0)
