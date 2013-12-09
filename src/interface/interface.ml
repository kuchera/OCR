let _ = GMain.init ()
let window =
  GWindow.window
    ~title:"Button box demo"
    ~position:`CENTER 
    ~resizable:true
    ~width:1000 ~height:1000 () 
  

let vbox = GPack.vbox 
  ~spacing:2 
  ~border_width:2
  ~packing:window#add ()

let hbox = GPack.hbox
	~spacing:2
	~packing:vbox#add


let scroll = GBin.scrolled_window
    ~height:200  
    ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:vbox#add () 


let img = ref ""


let image = 
    let buf = GdkPixbuf.create
        ~width:750
        ~height:600 () in
    GMisc.image
        ~pixbuf:buf
        ~packing:scroll#add_with_viewport () 

let view = GText.view 
	~packing:vbox#add 
	~width:50
	~height:100 ()


let x file  = img:=file;(*Sys.command ("./Nick "^ !img );*) print_endline !img
let change btn () = Gaux.may x btn#filename

(*let chooser = 
	let btn = GFile.chooser_button
	~action:`OPEN
	~packing:(vbox#pack ~expand:false) ()
	  in
          ignore (btn#connect#selection_changed (change btn ));
          image#set_file (!img)

*)



let bbox = GPack.button_box `HORIZONTAL
  ~layout:`EDGE (* C'est ici que l'on choisit la disposition des boutons. *)
  ~border_width:2
  ~packing:(vbox#pack ~expand:false) ()



let def x  = match x with
    |None -> ""
    |Some v -> v

let ask_for_file parent _ = 
        let dialog = GWindow.file_chooser_dialog
            ~action:`OPEN
            ~title:"Open a picture"
            ~parent  () in
            dialog#add_button_stock `CANCEL `CANCEL ;
            dialog#add_select_button_stock `OPEN `OPEN;
            begin match dialog#run () with
            |`OPEN -> 
                    img := (def dialog#filename);
                    image#set_file (!img)
            |`DELETE_EVENT | `CANCEL -> ()
            end;
            dialog#destroy ()


let _open = 
    let button = GButton.button
    ~stock:`OPEN
    ~packing:bbox#add () in
    ignore (button#connect#clicked ~callback: (ask_for_file window));
   button 

let binarize () = (ignore)(Sys.command ("./filtres niblack "^ !img ^" img.bmp"))
let binarisation = 
        let bin = GButton.button
         ~label:"Binarize"   
         ~packing:bbox#add () in
        bin#connect#clicked binarize;
        image#set_file 
            

let rot  () = (ignore)(Sys.command ("./rotation "^ !img ^ " img_rot.bmp"))
let rotation = 
        let bin = GButton.button
            ~label:"Rotation"   
            ~packing:bbox#add () in
        bin#connect#clicked rot 


let detect () = (ignore)(Sys.command ("./detection "^ !img))
let detection = 
        let bin = GButton.button
            ~label:"Detection"
            ~packing:bbox#add () in
        bin#connect#clicked detect

let r () = (ignore)(Sys.command ("./network "^ !img))
let read = 
        let bin = GButton.button
            ~label:"Read"
            ~packing:bbox#add () in
        bin#connect#clicked r 


let a () = (ignore)(Sys.command ("./all " ^ !img))
let all = 
        let bin = GButton.button
            ~label:"ALL"
            ~packing:bbox#add () in
        bin#connect#clicked a


let zoom scale =
    let s = scale in
    let pb0 = image#pixbuf in
    let w = int_of_float (float (GdkPixbuf.get_width pb0) *. s) in
    let h = int_of_float (float (GdkPixbuf.get_height pb0) *. s) in
    let pb = GdkPixbuf.from_file_at_size !img w h in
    image#set_pixbuf pb

let zoomin _ =
    if (!img <> "") then
        zoom 1.1
    else ()

let zoomout _ = 
    if(!img <> "") then
        zoom 0.9
    else ()


let zoom_in = 
        let zi = GButton.button
            ~stock:`ZOOM_IN
            ~packing:bbox#add () in
        ignore (zi#connect#clicked ~callback:zoomin)


let zoom_out = 
        let zo = GButton.button
            ~stock:`ZOOM_OUT
            ~packing:bbox#add () in
        ignore (zo#connect#clicked ~callback:zoomout)


let about_button = 
    let dlg = GWindow.about_dialog
    ~authors:["Eugene Kuchera";"Yassine Razani";"Alain Gbedo";"Arthur D'avray"]
    ~copyright: "Copyright @ 2013-2014 ShadowTeam"
    ~version: "2.0"
    ~title:"CREDITS"
    ~website:"http://www.optivision.com"
    ~position:`CENTER_ON_PARENT
    ~parent:window
    ~destroy_with_parent:true () in
        let btn = GButton.button 
            ~stock:`ABOUT
            ~packing:bbox#add () in
        let y = GMisc.image 
            ~stock:`ABOUT
            ~packing:btn#set_image () in 
            y#set_pixel_size 1;
            btn#connect#clicked (fun () -> ignore (dlg#run ()); dlg#misc#hide 
            ())
            

let destroy () = ignore (window#connect#destroy ~callback:GMain.quit) 
let quit =
	let qit = GButton.button 
	~stock:`QUIT 
	~packing:bbox#add () in
	qit#connect#clicked ~callback:GMain.quit 





let _ =
    destroy ();
    window#show ();
  GMain.main ()
