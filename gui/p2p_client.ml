(* TODO: Detect a client disconnect *)

(* TODO: Send a group of words to the raspberry pi, and ocaml code to calculate the count of words, then return the data back *)

let server_api = "http://localhost:8080/"

open Lwt.Infix

let (>>) = fun x y -> (x >>= fun _ -> y)

type user = {
  ip_address : string;
  port : string;
  username : string
}

(* NOTE: Simple set up for testing counting words in a string *)
type compute_request = {
  requestor_name : string;
  shell_command : string;
  script_name : string;
  script : string;
  data_name : string;
  data : string;
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

let user_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    ip_address  = member "ip_address" json_assoc |> to_string;
    port = member "port" json_assoc |> to_string;
    username = member "username" json_assoc |> to_string
  }

let json_of_compute_request c =
  "{\"requestor_name\" : \"" ^ c.requestor_name ^ "\", " ^
   "\"shell_command\" : \"" ^ c.shell_command ^ "\", " ^
   "\"script_name\" : \"" ^ c.script_name ^ "\", " ^
   "\"script\" : \"" ^ c.script ^ "\", " ^
   "\"data_name\" : \"" ^ c.data_name ^ "\", " ^
   "\"data\" : \"" ^ c.data ^ "\", " ^
  "}"

let compute_request_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    requestor_name = member "requestor_name" json_assoc |> to_string;
    shell_command = member "shell_command" json_assoc |> to_string;
    script_name = member "script_name" json_assoc |> to_string;
    script = member "script" json_assoc |> to_string;
    data_name = member "data_name" json_assoc |> to_string;
    data = member "data" json_assoc |> to_string
  }

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

module ClientLwtServer = struct

  open Lwt
  open Unix
  open Lwt_unix

  let server_port = 12345
  let so_timeout = Some 20
  let backlog = 10

  let hidden_file_path = ".p2p_compute"

  let make_hidden_dir () =
    try_lwt
      mkdir hidden_file_path 0o777
    with
    | Unix_error (EEXIST, _, _) -> Lwt.return ()
    | _ -> raise (Failure ("Failed to make directory " ^ hidden_file_path))

  let make_user_dir path =
    mkdir path 0o777

  let make_result_file path =
    lwt result_file = openfile (path ^ "result") [O_CREAT; O_TRUNC] 0o777 in
    close result_file

  let copy_script_file path cr =
    lwt script_file = openfile (path ^ cr.script_name) [O_WRONLY; O_CREAT] 0o777 in
    lwt script_write_result = write_string script_file cr.script 0 (String.length cr.script) in
    close script_file

  let copy_data_file path cr =
    lwt data_file = openfile (path ^ cr.data_name) [O_WRONLY; O_CREAT] 0o777 in
    lwt data_write_result = write_string data_file cr.data 0 (String.length cr.data) in
    close data_file

  let run_computation cr_json =
    let cr = compute_request_of_json cr_json in
    let p = hidden_file_path ^ "/" ^ cr.requestor_name ^ "/" in
    make_hidden_dir () >>
    make_user_dir p >>
    make_result_file p >>
    copy_script_file p cr >>
    copy_data_file p cr >>
    chdir p >>
    let cmd = Lwt_process.shell cr.shell_command in
    lwt p_status = Lwt_process.exec cmd in
    let exit_status =
      match p_status with
      | WEXITED i -> "Process Terminated Normally - Exit status " ^ (string_of_int i)
      | WSIGNALED _ -> "Process killed by a signal."
      | WSTOPPED _ -> "Process stopped by a signal."
    in
    Lwt.return exit_status

  (* Return the results in the result file if it exists *)
  let computation_results () = None

  (* TODO: Get computation result and put them into a JSON format *)
  (* TODO: Send the computation results back to the computation requestor *)
  (* TODO: Delete everything in the user directory when finished *)

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
    lwt server_addr = gethostbyaddr @@ inet_addr_of_string internal_ip in
    let sockaddr = ADDR_INET (server_addr.h_addr_list.(0), server_port) in
    let socket = init_socket sockaddr in
    Lwt_io.printl (
      "Listening for a socket connection on port " ^ (string_of_int server_port) ^
      " at ip " ^ (string_of_inet_addr server_addr.h_addr_list.(0))) >>
    (*Gui.username#set_text *)
    Lwt.return @@ Gui.internal_ip#set_text (string_of_inet_addr server_addr.h_addr_list.(0)) >>
    Lwt.return @@ Gui.internal_port_ip#set_text (string_of_int server_port) >>
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

let main () =
  Lwt.async ClientLwtServer.launch_process;
  Gui.main ()
