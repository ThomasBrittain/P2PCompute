let () = Lwt_glib.install ~mode:`glib_into_lwt ()

let locale = GtkMain.Main.init ()

let window =
  GWindow.window
    ~title:"P2P Compute"
    ~width:500
    ~height:800
    ~border_width:20
    ()

(* VBox to hold the client info *)
let client_info_vbox = GPack.vbox ~border_width:2 ~packing:window#add ()

(* Display the client's Username, IP and port *)
let client_info_table =
  GPack.table
    ~rows:3
    ~columns:2
    ~row_spacings:5
    ~col_spacings:5
    ~border_width:2
    ~packing:client_info_vbox#pack
    ()

let _ =
  GMisc.label
    ~markup:"<b>Username</b>"
    ~packing:(client_info_table#attach ~left:0 ~top:0 ~right:1 ~bottom:1)
    ()

let _ =
  GMisc.label
    ~markup:"<b>Internal IP</b>"
    ~packing:(client_info_table#attach ~left:0 ~top:1 ~right:1 ~bottom:2)
    ()

let _ =
  GMisc.label
    ~markup:"<b>Internal Port</b>"
    ~packing:(client_info_table#attach ~left:0 ~top:2 ~right:1 ~bottom:3)
    ()

let username =
  GMisc.label
    ~text:"username_goes_here"
    ~packing:(client_info_table#attach ~left:1 ~top:0 ~right:2 ~bottom:1)
    ()

let internal_ip =
  GMisc.label
    ~text:"internal_ip_goes_here"
    ~packing:(client_info_table#attach ~left:1 ~top:1 ~right:2 ~bottom:2)
    ()

let internal_port_ip =
  GMisc.label
    ~text:"internal_port_goes_here"
    ~packing:(client_info_table#attach ~left:1 ~top:2 ~right:2 ~bottom:3)
    ()

let main () =

  let waiter, wakener = Lwt.wait () in

  (* Kill the process when the user clicks the "X" button in top left cornet*)
  let _ = window#connect#destroy ~callback:GMain.Main.quit in

  window#show ();
  waiter
