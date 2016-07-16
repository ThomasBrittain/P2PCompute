(* TODO: Detect a client disconnect *)
(* TODO: After a node has finished a computation, need to be able to tell if it was               *)
(*       successfully received by the requestor or if it should be saved and re-sent again later *)

let server_api = "http://localhost:8080/"

open Lwt.Infix
open User_info

let (>>) = fun x y -> (x >>= fun _ -> y)

(* Data type for sending a computation request to another node *)
type compute_request = {
  requestor_name : string;
  requestor_ip : string;
  requestor_port: string;
  job_name : string;
  packet_num : int;
  shell_command : string;
  script_name : string;
  script : string;
  data_name : string;
  data : string
}

(* Data type for returning a computation result to another node *)
type compute_result = {
  job_name : string;
  job_created_at : float;
  packet_num : int;
  total_packets : int;
  result : string;
  exit_status : string
}

type client_msg =
  | ComputeReq of compute_request
  | ComputeRes of compute_result

(* Descrptive data about a computation made by the client *)
type job_info = {
  job_name : string;
  total_packets : int;
  packets_complete : int;
  created_at : float
}

(* Descriptive data about a single job that has been sent/received to/from another node *)
type job = {
  complete : bool;
  completed_by : user option;
  completed_at : float
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
   "\"data\" : \"" ^ c.data ^ "\"" ^
  "}"

let compute_request_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    requestor_name = member "requestor_name" json_assoc |> to_string;
    requestor_ip = member "requestor_ip" json_assoc |> to_string;
    requestor_port = member "requestor_port" json_assoc |> to_string;
    job_name = member "job_name" json_assoc |> to_string;
    packet_num = member "packet_num" json_assoc |> to_int;
    shell_command = member "shell_command" json_assoc |> to_string;
    script_name = member "script_name" json_assoc |> to_string;
    script = member "script" json_assoc |> to_string;
    data_name = member "data_name" json_assoc |> to_string;
    data = member "data" json_assoc |> to_string
  }

let compute_result_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    job_name = member "job_name" json_assoc |> to_string;
    job_created_at = member "job_created_at" json_assoc |> to_float;
    packet_num = member "packet_num" json_assoc |> to_int;
    total_packets = member "total_packets" json_assoc |> to_int;
    result = member "result" json_assoc |> to_string;
    exit_status = member "exit_status" json_assoc |> to_string
  }

let json_of_job_info (j : job_info) =
  "{\"job_name\" : \"" ^ j.job_name ^ "\", " ^
   "\"total_packets\" : \"" ^ (string_of_int j.total_packets) ^ "\"" ^
   "\"packets_complete\" : \"" ^ (string_of_int j.packets_complete) ^ "\"" ^
   "\"created_at\" : \"" ^ (string_of_float j.created_at) ^ "\"" ^
  "}"

let job_info_of_json json_assoc =
  let open Yojson.Basic.Util in
  {
    job_name = member "job_name" json_assoc |> to_string;
    total_packets = member "total_packets" json_assoc |> to_int;
    packets_complete = member "packets_complete" json_assoc |> to_int;
    created_at = member "created_at" json_assoc |> to_float;
  }

let client_msg_of_string s =
  let open Yojson.Basic in
  let json_assoc = from_string s in
  try
    let first_key = Util.to_assoc json_assoc |> List.hd |> fst in
    match first_key with
    | "requestor_name" -> ComputeReq (compute_request_of_json json_assoc)
    | "result" -> ComputeRes (compute_result_of_json json_assoc)
    | _ -> raise (Failure "invalid json")
  with
  | _ -> raise (Failure "invalid json")

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

module ClientLwtServer = struct

  open Lwt
  open Unix
  open Lwt_unix

  let so_timeout = Some 20
  let backlog = 10

  let hidden_file_path = ".p2p_compute"

  let make_dir path =
    try_lwt
      mkdir path 0o777
    with
    | Unix_error (EEXIST, _, _) -> Lwt.return_unit
    | _ -> raise (Failure ("Failed to make directory" ^ path))

  let make_result_file path =
    lwt result_file = openfile (path ^ "result") [O_CREAT; O_TRUNC] 0o777 in
    close result_file

  (* TODO: Combine copy_script_file and copy_data_file into a single directory *)
  let copy_script_file path cr =
    lwt script_file = openfile (path ^ cr.script_name) [O_WRONLY; O_CREAT] 0o777 in
    lwt script_write_result = write_string script_file cr.script 0 (String.length cr.script) in
    close script_file

  let copy_data_file path cr =
    lwt data_file = openfile (path ^ cr.data_name) [O_WRONLY; O_CREAT] 0o777 in
    lwt data_write_result = write_string data_file cr.data 0 (String.length cr.data) in
    close data_file

  let write_to_file ~path s =
    lwt file_handle = openfile path [O_WRONLY; O_CREAT] 0o777 in
    lwt _ = write_string file_handle s 0 (String.length s) in
    close file_handle

  let rec read_all ?(result = "") ic () =
    Lwt_io.printl "read_all called" >>= Lwt_io.flush_all >>
    let open Lwt_io in
    try_lwt
      lwt new_line = read_line ic in
      read_all ~result:(result ^ new_line) ic ()
    with
    | End_of_file -> close ic >|= fun () -> result

  let rec dir_contents ?(contents = []) dir_ic =
    try_lwt
      lwt e = readdir dir_ic in
      dir_contents ~contents:(e :: contents) dir_ic
    with
    | End_of_file -> closedir dir_ic >> Lwt.return contents

  let create_job_info_file job_info =
    let job_dir = job_info.job_name ^ "_" ^ (string_of_float job_info.created_at) ^ ".json" in
    write_to_file
      ~path:(hidden_file_path ^ "/jobs/" ^ job_dir ^ "/job_info.json")
      (json_of_job_info job_info)

  let returned_packets (cpt_res : compute_result) =
    let path =
      hidden_file_path ^ "/jobs/" ^ cpt_res.job_name ^ "_" ^
      (string_of_float cpt_res.job_created_at) ^ "/results"
    in
    lwt dir = opendir path in
    lwt dir_files = dir_contents dir in
    List.filter
      (fun s -> Str.string_match (Str.regexp "results_packet_[1-9][0-9]*.json") s 0)
      dir_files
  |> List.length |> Lwt.return

  (* Update a job info file based on the data already stored *)
  let update_job_info_file (cpt_res : compute_result) =
    (* check if the file exists, if not, the create the file *)
    let path = hidden_file_path ^ "/jobs/" ^ cpt_res.job_name  ^ ".json" in
    try_lwt
      lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input path in
      lwt job_info_json =
        read_all ic ()
        >|= Yojson.Basic.from_string
        >|= job_info_of_json
      in
      lwt oc =
        Lwt_io.open_file ~flags:[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o777 ~mode:Lwt_io.Output path
      in
      lwt pkts = returned_packets cpt_res in
      let new_file_data =
        Lwt_stream.of_string @@ json_of_job_info {
          job_name = cpt_res.job_name;
          total_packets = cpt_res.total_packets;
          packets_complete = pkts;
          created_at = cpt_res.job_created_at
        }
      in
      Lwt_io.write_chars oc new_file_data >>
      Lwt_io.flush oc >>
      Lwt_io.close oc
    with
    | Unix_error (ENOENT, _, _) ->
      create_job_info_file {
        job_name = cpt_res.job_name;
        total_packets = cpt_res.total_packets;
        packets_complete = 1;
        created_at = Unix.time ()
      }

  let run_computation cr_json =
    Lwt_io.printl "Entered run_computation" >>= Lwt_io.flush_all >>
    let cr = compute_request_of_json @@ Yojson.Basic.from_string cr_json in
    Lwt_io.printl "Json parsed!" >>= Lwt_io.flush_all >>
    let p = hidden_file_path ^ "/" ^ cr.requestor_name ^ "/" in
    make_dir hidden_file_path >>
    make_dir p >>
    make_result_file p >>
    copy_script_file p cr >>
    copy_data_file p cr >>
    chdir p >>
    (* TODO: Add ability to disable the process' networking and all access to other file *)
    let cmd = Lwt_process.shell cr.shell_command in
    lwt p_status = Lwt_process.exec cmd in
    let exit_status =
      match p_status with
      | WEXITED i -> "Process Terminated Normally - Exit status " ^ (string_of_int i)
      | WSIGNALED _ -> "Process killed by a signal."
      | WSTOPPED _ -> "Process stopped by a signal."
    in
    Lwt_io.printl "run_computatin is complete" >>= Lwt_io.flush_all >>
    Lwt.return exit_status

  (* Return the results in the result file if it exists *)
  let computation_result cr_json () =
    let open Lwt_io in
    lwt exit_status = run_computation cr_json in
    Lwt_io.print exit_status >>=
    Lwt_io.flush_all >>
    lwt ic = open_file ~mode:Input "result" in
    lwt result = read_all ic () in
    Lwt.return @@
    "{\"result\" : " ^ result ^ "," ^
    "\"exit_status\" : " ^ exit_status ^ "}"

  (* Store the result of a computation returned from another node *)
  let store_computation_result (cpt_res : compute_result) =
    (* TODO: Check if all packets have been returned and update the job info file accordingly *)
    make_dir hidden_file_path >>
    make_dir (hidden_file_path ^ "/jobs") >>
    make_dir (hidden_file_path ^ "/jobs/" ^ cpt_res.job_name) >>
    (* TODO: Create the job info file and write its contents *)
    make_dir (hidden_file_path ^ "/jobs/" ^ cpt_res.job_name ^ "/results") >>
    let result_path =
      hidden_file_path ^ "/jobs/" ^ cpt_res.job_name ^
      "/results/" ^ (string_of_int cpt_res.packet_num)
    in
    write_to_file ~path:result_path cpt_res.result

  (* TODO: Send the computation results back to the computation requestor *)
  (* TODO: Delete everything in the user directory when finished *)
  (* TODO:
     User Clicks Gui Button "Run Computation" with form filled out
     --> send the computation to another computer
     --> receive all results from ClientLwtServer then wait to receive the results
  *)

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
          (*let c = callback inchan outchan in
          let events =
            match timeout with
            | None -> [c]
            | Some t -> [c; sleep (float_of_int t) >> return ()]
            in
            ignore (Lwt.pick events >> try_close outchan >> try_close inchan);*)
          callback inchan outchan >>
          _process ()
        )
    in
    _process ()

  let launch_process () =
    let port = int_of_string (User_info).!my_node.port in
    let ic, oc = Unix.open_process "hostname -I" in
    let ip_info = input_line ic in
    close_in ic;
    close_out oc;
    let internal_ip = Str.split (Str.regexp "[ ]") ip_info |> List.hd in
    lwt server_addr = gethostbyaddr @@ inet_addr_of_string internal_ip in
    let sockaddr = ADDR_INET (server_addr.h_addr_list.(0), port) in
    let socket = init_socket sockaddr in
    Lwt_io.printl (
      "Listening for a socket connection on port " ^ (string_of_int port) ^
      " at ip " ^ (string_of_inet_addr server_addr.h_addr_list.(0))) >>
    Lwt.return @@ Gui.internal_ip#set_text (string_of_inet_addr server_addr.h_addr_list.(0)) >>
    process
    socket
    ~timeout:so_timeout
    ~callback: (
      fun inchan outchan ->
        lwt msg = read_all inchan () in
        Lwt_io.printl msg >>= Lwt_io.flush_all >>
        match client_msg_of_string msg with
        | ComputeReq cr ->
          computation_result msg ()
          >>= ClientToClient.send_msg ~ip:cr.requestor_ip ~port:cr.requestor_port
        | ComputeRes cr -> store_computation_result cr
    )

end

(* To test messages on localhost *)
module ClientMsgSendLocalhost = struct

  open Lwt_unix

  lwt hentry = gethostbyname "localhost"

  let send_msg msg =
    let port = int_of_string (User_info).!my_node.port in
    let client_sock = socket PF_INET SOCK_STREAM 0 in
    connect client_sock (ADDR_INET (hentry.h_addr_list.(0), port))
    >>= fun () -> send client_sock (Bytes.of_string msg) 0 (String.length msg) []
    >>= fun _ -> close client_sock

  let rec chat () =
    Lwt_io.read_line Lwt_io.stdin
    >>= send_msg
    >>= chat

end

(* Send a message to all other nodes *)
let send_to_all_nodes msg =
  let open ClientToClient in
  register_node !my_node >>
  lwt all_nodes = get_all_nodes () in
  Lwt_list.iter_p
    (fun (u : user) -> send_msg ~ip:u.ip_address ~port:u.port ("Hello from " ^ u.username))
    all_nodes
