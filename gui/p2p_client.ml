(* TODO: Detect a client disconnect *)

let server_api = "http://localhost:8080/"

open Lwt.Infix

let (>>) = fun x y -> (x >>= fun _ -> y)

type user = {
  ip_address : string;
  port : string;
  username : string
}

let my_node : user ref = ref {
  ip_address = "";
  port = "";
  username =""
}

let json_of_user u =
  "  {\"ip_address\" : \"" ^ u.ip_address ^ "\", " ^
  "\"port\" : \"" ^ u.port ^ "\", " ^
  "\"username\" : \"" ^ u.username ^ "\"" ^
  "}"

(* Get request *)
let get_it path =
  let get_uri = Uri.of_string (server_api ^ path) in
  Cohttp_lwt_unix.Client.get get_uri
  >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
  >|= fun s -> Yojson.Basic.from_string s

(* Post request *)
let post_it ~body path =
  let post_uri = Uri.of_string (server_api ^ path) in
  let post_header =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
  in
  Cohttp_lwt_unix.Client.post ~headers:post_header ~body:(Cohttp_lwt_body.of_string body) post_uri
  >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
  >|= fun s -> Yojson.Basic.from_string s

let user_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    ip_address  = member "ip_address" json_assoc |> to_string;
    port = member "port" json_assoc |> to_string;
    username = member "username" json_assoc |> to_string
  }

(* Get a list of available nodes *)
let get_all_nodes () =
  let open Yojson.Basic.Util in
  get_it "available_nodes"
  >|= member "active_users"
  >|= to_list
  >|= List.map user_of_json

(* Add the client to the list of nodes *)
let register_node u =
  post_it ~body:(json_of_user u) "add_node"

(* TODO: Get a client socket set up, and get communication working between two clients *)
(* Each client should always have an open socket for both reading and writing *)

module ClientLwtServer = struct

  open Lwt
  open Unix
  open Lwt_unix

  let server_port = 12345
  let so_timeout = Some 20
  let backlog = 10

  let try_close chan =
    catch (fun () -> Lwt_io.close chan)
    (function _ -> return ())

  let init_socket sockaddr =
    let socket = socket PF_INET SOCK_STREAM 0 in
    setsockopt socket SO_REUSEADDR true;
    bind socket sockaddr;
    listen socket backlog;
    socket

  let process socket ~timeout ~callback =
    let rec _process () =
      Lwt_io.printl "Listening for a socket connection..." >>
      accept socket >>=
        (fun (socket_cli, _) ->
          let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
          let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
          let c = callback inchan outchan in
          let events =
            match timeout with
            | None -> [c]
            | Some t -> [c; sleep (float_of_int t) >> return ()]
          in
          ignore (Lwt.pick events >> try_close outchan >> try_close inchan);
          _process ()
        )
    in
    _process ()

  let launch_process () =
    let ic, oc = Unix.open_process "hostname -I" in
    let ip_info = input_line ic in
    close_in ic;
    close_out oc;
    let internal_ip = Str.split (Str.regexp "[ ]") ip_info |> List.hd in
    lwt server_addr = gethostbyaddr @@ inet_addr_of_string internal_ip (*"192.168.0.8"*) in
    let sockaddr = ADDR_INET (server_addr.h_addr_list.(0), server_port) in
    let socket = init_socket sockaddr in
    Lwt_main.run (
      Lwt_io.printl (
        "Listening for a socket connection on port " ^ (string_of_int server_port) ^
        " at ip " ^ (string_of_inet_addr server_addr.h_addr_list.(0))) >>
      process
      socket
      ~timeout:so_timeout
      ~callback:
        (fun inchan outchan ->
           Lwt_io.read_line inchan
           >>= fun msg -> Lwt_io.printl msg
           >>= Lwt_io.flush_all)
        (*fun inchan outchan ->
          Lwt_io.read_line inchan >>= (fun msg -> Lwt_io.write_line outchan msg)*)
    )

end

(* To test messages on localhost *)
module ClientMsgSendLocalhost = struct

  open Lwt_unix

  lwt hentry = gethostbyname "localhost"

  let send_msg msg =
    let client_sock = socket PF_INET SOCK_STREAM 0 in
    connect client_sock (ADDR_INET (hentry.h_addr_list.(0), ClientLwtServer.server_port))
    >>= fun () -> send client_sock (Bytes.of_string msg) 0 (String.length msg) []
    >>= fun _ -> close client_sock

  let rec chat () =
    Lwt_io.read_line Lwt_io.stdin
    >>= send_msg
    >>= chat

end

(* Communication between client nodes *)
module ClientToClient = struct

  open Lwt_unix

  let send_msg ~ip ~port msg =
    lwt hentry = gethostbyaddr @@ Unix.inet_addr_of_string ip in
    let client_sock = socket PF_INET SOCK_STREAM 0 in
    connect client_sock (ADDR_INET (hentry.h_addr_list.(0), int_of_string @@ port))
    >>= fun () -> send client_sock (Bytes.of_string msg) 0 (String.length msg) []
    >>= fun _ -> close client_sock

end

(* Send a message to all other nodes *)
let send_to_all_nodes msg =
  let open ClientToClient in
  my_node := {
    ip_address = "";
    port = "1234";
    username ="system76"
  };
  register_node !my_node >>
  lwt all_nodes = get_all_nodes () in
  Lwt_list.iter_p
    (fun (u : user) -> send_msg ~ip:u.ip_address ~port:u.port ("Hello from " ^ u.username))
    all_nodes
