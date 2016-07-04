{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D

  type user = {
    ip_address : string;
    port : string;
    username : string
  }

}}

{server{
  let active_users : (string, user) Hashtbl.t = Hashtbl.create 100

  let rec add_user u =
    try (
      if Hashtbl.mem active_users u.username
      then (Hashtbl.remove active_users u.username; add_user u)
      else Hashtbl.add active_users u.username u
    )
    with
    | Not_found -> Hashtbl.add active_users u.username u

  let remove_user u = Hashtbl.remove active_users u.username

  let get_all_users () = Hashtbl.fold (fun k v acc -> v :: acc) active_users []

  let json_of_user u =
    "  {\"ip_address\" : \"" ^ u.ip_address ^ "\", " ^
    "\"port\" : \"" ^ u.port ^ "\", " ^
    "\"username\" : \"" ^ u.username ^ "\"" ^
    "}"

  let json_of_users users =
    let users_list = List.map (json_of_user) (get_all_users ()) in
    "{\"active_users\" : [\n" ^ (String.concat ",\n" users_list) ^ "\n]}"

  let user_of_json json_assoc =
    let open Yojson.Basic.Util in
    {
      ip_address  = member "ip_address" json_assoc |> to_string;
      port = member "port" json_assoc |> to_string;
      username = member "username" json_assoc |> to_string
    }

}}

module P2p_compute_app =
  Eliom_registration.App (
    struct
      let application_name = "p2p_compute"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let
  () =
  P2p_compute_app.register
    ~service:main_service
    (fun () () ->
      let ip = Eliom_request_info.get_remote_ip () in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"p2p_compute"
           ~css:[["css";"p2p_compute.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
             h2 [pcdata ("Your ip address is: " ^ ip)];
           ])))

let available_nodes_service =
  Eliom_service.Http.service ~path:["available_nodes"] ~get_params:Eliom_parameter.unit ()

let add_node_fallback =
  Eliom_service.App.service
    ~path:["add_node"]
    ~get_params:Eliom_parameter.unit
    ()

let
  () =
  P2p_compute_app.register
    ~service:add_node_fallback
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"p2p_compute"
           ~css:[["css";"p2p_compute.css"]]
           Html5.F.(
             body
             [h2
              [pcdata "add_node fallback page"];
             ]
           )
        )
    )

let add_node_service =
  Eliom_service.Http.post_service
    ~fallback:add_node_fallback
    ~post_params:Eliom_parameter.raw_post_data
    ()

let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let check_content_type ~mime_type content_type =
  match content_type with
  | Some ((type_, subtype), _) when (type_ ^ "/" ^ subtype) = mime_type -> true
  | _ -> false

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream

let available_nodes_handler () () =
  send_json ~code:200 (json_of_users @@ get_all_users ())

let add_node user_ip (content_type, raw_content_opt) =
  if not (check_content_type ~mime_type:json_mime_type content_type)
  then send_json ~code:400 "{\"error\" : \"Content type is wrong, it must be JSON.\"}"
  else
    match raw_content_opt with
    | None -> send_json ~code:400 "{\"error\" : \"Body content is missing.\"}"
    | Some raw_content ->
      read_raw_content raw_content
      >>= fun s ->
      try (
        let json = Yojson.Basic.from_string s in
        let u = user_of_json json in
        add_user {
          ip_address = user_ip; (* Override the users sent ip_address with what the server sees *)
          port = u.port;
          username = u.username
        };
        send_json ~code:200 "{\"accepted\" : true}"
      )
      with
      | _ -> send_json ~code:200 "Fail"

let add_node_handler () content =
  let ip = Eliom_request_info.get_remote_ip () in
  add_node ip content

let () =
  Eliom_registration.Any.register available_nodes_service available_nodes_handler;
  Eliom_registration.Any.register add_node_service add_node_handler
