(* TODO: Try to look up username and port from the last used values stored somewhere *)

open User_info

let () = Lwt_glib.install ~mode:`glib_into_lwt ()

let locale = GtkMain.Main.init ()

let login_window =
  GWindow.window
    ~title:"Log in"
    ~width:500
    ~height:400
    ~border_width:20
    ()

let login_vbox = GPack.vbox ~border_width:2 ~packing:login_window#add ()

let login_table =
  GPack.table
    ~rows:3
    ~columns:2
    ~row_spacings:5
    ~col_spacings:5
    ~border_width:2
    ~packing:login_vbox#pack
    ()

let _ =
  GMisc.label
    ~markup:"<b>Username</b>"
    ~packing:(login_table#attach ~left:0 ~top:0 ~right:1 ~bottom:1)
    ()

let username_field =
  GEdit.entry
    ~visibility:true
    ~max_length:100
    ~editable:true
    ~width:100
    ~height:30
    ~packing:(login_table#attach ~left:1 ~top:0 ~right:2 ~bottom:1)
    ()

let _ =
  GMisc.label
    ~markup:"<b>Port</b>"
    ~packing:(login_table#attach ~left:0 ~top:1 ~right:1 ~bottom:2)
    ()

let port_field =
  GEdit.entry
    ~visibility:true
    ~max_length:100
    ~editable:true
    ~width:100
    ~height:30
    ~packing:(login_table#attach ~left:1 ~top:1 ~right:2 ~bottom:2)
    ()

let login_submit_btn =
  GButton.button
    ~label:"Log In"
    ~packing:(login_table#attach ~left:0 ~top:2 ~right:2 ~bottom:3)
    ~show:true
    ()

(* Main Window Section *)
let window =
  GWindow.window
    ~title:"P2P Compute"
    ~width:500
    ~height:400
    ~border_width:20
    ()

(* VBox to hold the client info *)
let client_info_vbox = GPack.vbox ~border_width:2 ~packing:window#add ()

(* Main Window Menubar*)
let mb = GMenu.menu_bar ~packing:client_info_vbox#pack ()
let mb_factory = new GMenu.factory mb
let mb_accel_group = mb_factory#accel_group
let mb_file_menu = mb_factory#add_submenu "Item Goes Here"

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
    ~text:(User_info).!my_node.username
    ~packing:(client_info_table#attach ~left:1 ~top:0 ~right:2 ~bottom:1)
    ()

let internal_ip =
  GMisc.label
    ~text:"internal_ip_goes_here"
    ~packing:(client_info_table#attach ~left:1 ~top:1 ~right:2 ~bottom:2)
    ()

let internal_port =
  GMisc.label
    ~text:"internal_port_goes_here"
    ~packing:(client_info_table#attach ~left:1 ~top:2 ~right:2 ~bottom:3)
    ()

let main () =

  let waiter, wakener = Lwt.wait () in

  (* Kill the process when the user clicks the "X" button in top left corner *)
  (* TODO: This only kills the gui process, but it needs to kill all Lwt processes! *)
  (*let _ = window#connect#destroy ~callback:GMain.Main.quit in*)
  ignore (window#connect#destroy (Lwt.wakeup wakener));

  login_window#show ();

  waiter
