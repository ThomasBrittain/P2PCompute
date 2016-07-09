open Gui
open User_info

let _ =
  Gui.login_submit_btn#connect#clicked
    ~callback:(
      fun () ->
        User_info.my_node := {
          ip_address = !my_node.ip_address;
          port = port_field#text;
          username = username_field#text
        };
        Gui.username#set_text (User_info).!my_node.username;
        Gui.internal_port#set_text (User_info).!my_node.port;
        Gui.login_window#destroy ();
        Lwt.async @@ P2p_client.ClientLwtServer.launch_process;
        Gui.window#show ()
      )
