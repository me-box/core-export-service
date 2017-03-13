open Lwt
open Opium.Std

module Macaroon = Sodium_macaroons
module Client   = Cohttp_lwt_unix.Client
module M        = Test_engine
module R        = Rresult.R


let arbiter_endp         = "http://127.0.0.1:8888"
let arbiter_token        = "Believe it or not, I'm an arbiter token"
let macaroon_secret      = "Am I a secret, or not, or whatever?"
let export_service       = "data_export_service"
let export_port          = "8080"
let local_echo_port      = "8000"
let local_echo_slow_port = "8008"


let set_environment () =
  Unix.putenv "DATABOX_LOCAL_NAME" export_service;
  Unix.putenv "DATABOX_LOCAL_PORT" export_port;
  Unix.putenv "DATABOX_ARBITER_ENDPOINT" arbiter_endp;
  Unix.putenv "ARBITER_TOKEN" arbiter_token


let logging_mw service =
  let filter = fun handler req ->
    let meth =
      Request.meth req
      |> Cohttp.Code.string_of_method
    in
    let uri = Request.uri req in
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun b ->
    Logs_lwt.info (fun m ->
        m "[%s] %s http:%a %s" service meth Uri.pp_hum uri b) >>= fun () ->
    let b = Cohttp_lwt_body.of_string b in
    let req = Request.({req with body = b}) in
    handler req
  in
  Opium_rock.Middleware.create ~filter ~name:"logging request middleware"


let local_echo () =
  let echo = post "/" begin fun req ->
      let body = Request.body req in
      Cohttp_lwt_body.to_string body >>= fun b ->
      let v = Ezjsonm.(from_string b |> value) in
      let r = `O ["request", v] in
      let s = Ezjsonm.to_string r in
      `String s |> respond' end
  in
  let app =
    App.empty
    |> App.port (int_of_string local_echo_port)
    |> App.middleware (logging_mw "local_echo")
    |> echo
  in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let local_echo_slow () =
  let echo = post "/" begin fun req ->
      let body = Request.body req in
      Cohttp_lwt_body.to_string body >>= fun b ->
      let v = Ezjsonm.(from_string b |> value) in
      let r = `O ["request", v] in
      let s = Ezjsonm.to_string r in
      return @@`String s >>= fun resp ->
      Lwt_unix.sleep 3.0 >>= fun () ->
      Logs_lwt.info (fun m ->
          m "[echo_slow] responding with %s" s) >>= fun () ->
      respond' resp end
  in
  let app =
    App.empty
    |> App.port (int_of_string local_echo_slow_port)
    |> App.middleware (logging_mw "local_echo_slow")
    |> echo
  in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let mint_macaroon ?(id = "arbiter") ?(location = arbiter_endp) ?(key = macaroon_secret)
    ?(target = "target = " ^ export_service) ~routes () =
  let m = Macaroon.create ~id ~location ~key in
  let m = Macaroon.add_first_party_caveat m target in
  let m = Macaroon.add_first_party_caveat m routes in
  Macaroon.serialize m


let arbiter () =
  let secret_endp = get "/store/secret" begin fun req ->
      let headers = Request.headers req in
      let api_key = Cohttp.Header.get headers "X-Api-Key" in
      if not (api_key = Some arbiter_token) then
        let code = `Unauthorized in
        `String "Missing/Invalid API key" |> respond' ~code
      else
      let s = B64.(encode ~alphabet:uri_safe_alphabet macaroon_secret) in
      `String s |> respond'
    end in
  let token_endp = get "/token" begin fun req ->
      let routes = Ezjsonm.(
          let l = `A [`String "/export"; `String "/lp/export"] in
          let d = `O ["POST", l] in
          let v = to_string d in
          "routes = " ^ v
        ) in
      let token = mint_macaroon ~routes () in
      `String token |> respond'
    end in
  let invalid_routes_token_endp = get "/routes-token" begin fun req ->
      let routes = Ezjsonm.(
          let l = `A [] in
          let d = `O ["POST", l] in
          let v = to_string d in
          "routes = " ^ v)
      in
      let token = mint_macaroon ~routes () in
      `String token |> respond'
    end in
  let app =
    App.empty
    |> App.middleware (logging_mw "arbiter")
    |> App.port 8888
    |> secret_endp
    |> token_endp
    |> invalid_routes_token_endp in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let server () =
  set_environment ();
  Lwt.join [arbiter (); Export.t (); local_echo (); local_echo_slow ()]



let get_field b field f =
  let open Ezjsonm in
  let obj = value @@ from_string b in
  let dic = get_dict obj in
  f @@ List.assoc field dic
  |> return

let data = `O ["key", `String "KEY0"; "value", `A [`String "V0"; `String "V1"]]

let make_body ?uri id =
  let uri = match uri with
  | None -> "http://127.0.0.1:" ^ local_echo_port
  | Some uri -> uri
  in
  let obj = `O [
      "id",   `String id;
      "uri", `String uri;
      "data", `String (Ezjsonm.to_string data); ]
  in
  obj
  |> Ezjsonm.to_string


module Test_client' = struct

  let env = Hashtbl.create 13
  let put_env k v = Hashtbl.add env k v
  let get_env k =
    if not (Hashtbl.mem env k) then None
    else Some (Hashtbl.find env k)


  let step meth uri ?headers ?body ?pre ?post () =
    let uri = Uri.of_string uri in
    let headers = match headers with
    | None -> Cohttp.Header.init ()
    | Some h -> Cohttp.Header.of_list h
    in
    let body = match body with
    | None -> Cohttp_lwt_body.empty
    | Some b -> Cohttp_lwt_body.of_string b
    in

    (match pre with
    | None -> return (uri, headers, body)
    | Some f -> f uri headers body)

    >>= fun (uri, headers, body) ->
    (match meth with
    | `GET -> Client.get ~headers uri
    | `POST -> Client.post ~headers ~body uri
    | _ -> Lwt.fail_with "not implemented METHOD")

    >>= fun (resp, body) ->
    match post with
    | None -> return_unit
    | Some f ->
        f resp body >>= fun _ ->
        return_unit


  let flow steps =
    Lwt_list.iteri_s (fun i ((m, u, headers, body, pre, post) as s) ->
        let pp_step ppf (m, u, h, b, _, _) =
          let m = Cohttp.Code.string_of_method m in
          Format.fprintf ppf "%s %s" m u
        in
        Logs_lwt.info (fun m -> m "[client] step %d %a" i pp_step s) >>= fun () ->
        step m u ?headers ?body ?pre ?post ()) steps


  let assertion ?resp ?body () =
    (match resp with
    | Some (r, ext_r, exp_r, pp_r, k_r) ->
        ext_r r >>= fun re_r ->
        if exp_r = re_r then k_r re_r else
        Logs_lwt.err (fun m ->
            m "Assertion failed, expected %a, got %a" pp_r exp_r pp_r re_r)
        >>= fun () -> Lwt.fail_with "assertion failed"
    | None -> return_unit) >>= fun () ->

    match body with
    | Some (b, ext_b, exp_b, pp_b, k_b) ->
        ext_b b >>= fun re_b ->
        if exp_b = re_b then k_b re_b else
        Logs_lwt.err (fun m ->
            m "Assertion failed, expected %a, got %a" pp_b exp_b pp_b re_b)
        >>= fun () -> Lwt.fail_with "assertion failed"
    | None -> return_unit


  let default_k = fun _ -> return_unit

  let extract_status s = return @@ Cohttp.Response.status s

  let pp_status ppf s =
    let s = Cohttp.Code.string_of_status s in
    Format.fprintf ppf "%s" s


  let save_token r b =
    Cohttp_lwt_body.to_string b >>= fun b ->

    let resp =
      let exp_r = `OK in
      r, extract_status, exp_r, pp_status, default_k
    in
    let body =
      let ext_b = fun i -> return i in
      let exp_b = b in
      let k b =
        put_env "x-api-key" b;
        return_unit in
      b, ext_b, exp_b, Format.pp_print_string, k
    in
    assertion ~resp ~body ()


  let insert_token u h b =
    match get_env "x-api-key" with
    | None -> return (u, h, b)
    | Some k ->
        Logs_lwt.info (fun m -> m "[client] get key: %s" k)
        >>= fun () ->
        let h = Cohttp.Header.add h "X-Api-Key" k in
        return (u, h, b)


  let print s b =
    extract_status s >>= fun s ->
    Cohttp_lwt_body.to_string b >>= fun b ->
    let s = Cohttp.Code.string_of_status s in
    Logs_lwt.info (fun m -> m "[client] status: %s body: %s" s b)


  let case0 = [
    `GET, arbiter_endp ^ "/token",
    None, None, None, Some save_token;
    `POST, "http://127.0.0.1:" ^ export_port ^ "/export",
    None, Some (make_body ""), Some insert_token, Some print;
  ]


  let client () =
    Lwt_list.iter_s flow [case0]

end


module Test_client = struct

  let client () =
    let uri = Uri.of_string arbiter_endp in
    let uri = Uri.with_path uri "/token" in
    Logs_lwt.app (fun m -> m "[client] GET %a" Uri.pp_hum uri) >>= fun () ->
    Client.get uri >>= fun (_, body)->
    Cohttp_lwt_body.to_string body >>= fun m ->
    Logs_lwt.app (fun m' -> m' "[client] %s" m) >>= fun () ->

    let headers = Cohttp.Header.init_with "X-Api-Key" m in
    let uri = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
    let uri = Uri.with_path uri "/export" in

    let body = make_body "" |> Cohttp_lwt_body.of_string in
    Client.post ~body ~headers uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    let status = Cohttp.Response.status resp in
    (if status != `OK then
       let status = Cohttp.Code.string_of_status status in
       Logs_lwt.err (fun m -> m "[client] status: %s" status) >>= fun () ->
       Logs_lwt.err (fun m -> m "[client] body: %s" body)
     else return_unit) >>= fun () ->

    let () = assert (status = `OK) in
    get_field body "id" Ezjsonm.get_string   >>= fun id ->
    get_field body "state" Ezjsonm.get_string >>= fun state ->
    let () = assert (state = "Pending") in

    let rec aux b s =
      Logs_lwt.app (fun m -> m "[client] state:%s body:%s" s b) >>= fun () ->
      match s with
      | "Finished" ->
          let open Ezjsonm in
          get_field b "ext_response" get_dict >>= fun response ->
          let status = get_string @@ List.assoc "status" response in
          let () = assert (status = "200 OK") in
          let dic =
            List.assoc "body" response
            |> get_string
            |> from_string
            |> value
            |> get_dict
            |> List.assoc "request"
            |> get_dict
          in
          let () = assert (List.mem_assoc "key" dic) in
          let () = assert (List.mem_assoc "value" dic) in
          let key = List.assoc "key" dic |> get_string in
          let values = List.assoc "value" dic |> get_list get_string in
          let () = assert (key = "KEY0") in
          let () = assert (values = ["V0"; "V1"]) in
          return_unit
      | _ ->
          Lwt_unix.sleep 0.5 >>= fun () ->
          let body = make_body id |> Cohttp_lwt_body.of_string in
          Client.post ~body ~headers uri >>= fun (resp, body) ->
          Cohttp_lwt_body.to_string body >>= fun body ->
          get_field body "state" Ezjsonm.get_string >>= fun state ->
          aux body state
    in
    aux body state

end


module Test_client'' = struct

  let case0 =
    let name = "basic test" in
    let step0 =
      let uri  =
        let uri' = Uri.of_string arbiter_endp in
        Uri.with_path uri' "/token"
      in
      let after_ts resp body =
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`OK s M.pp_status >>= fun r ->
        R.bind r (fun () ->
            M.put_env "x-api-key" body;
            R.ok ())
        |> return
      in
      M.({meth = `GET; uri;
          before_ts = empty_before_ts; after_ts;
          next_ts = no_next_step})
    in
    let rec step1 =
      let uri =
        let uri' = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
        Uri.with_path uri' "/export"
      in
      let before_ts () =
        let key =
          match M.get_env "x-api-key" with
          | None -> ""
          | Some k -> k
        in
        let h = Cohttp.Header.init_with "X-Api-Key" key in
        let id =
          match M.get_env "id" with
          | None -> ""
          | Some id -> id
        in
        let b =
          make_body id
          |> Cohttp_lwt_body.of_string
        in
        return (h, b)
      in
      let after_ts resp body =
        M.print_after_ts (resp, body) >>= fun _ ->
        get_field body "state" Ezjsonm.get_string >>= fun s ->

        let s = String.lowercase_ascii s in
        if not (s = "finished") then
          get_field body "id" Ezjsonm.get_string >>= fun id ->
          M.put_env "id" id;
          M.rm_env "ext_resp";
          return_ok ()
        else
        get_field body "ext_response" Ezjsonm.get_dict >>= fun ext_resp ->
        let v =
          `O ext_resp
          |> Ezjsonm.to_string
        in
        M.put_env "ext_resp" v;
        return_ok ()
      in
      let next_ts () =
        match M.get_env "ext_resp" with
        | None -> return_some step1
        | Some ext_resp ->
            let ext_resp = Ezjsonm.(
                from_string ext_resp
                |> value
                |> get_dict)
            in
            let data' =
              let open Ezjsonm in
              List.assoc "body" ext_resp
              |> get_string
              |> from_string
              |> value
              |> get_dict
              |> List.assoc "request"
            in
            let pp_data ppf d =
              let d = Ezjsonm.wrap d in
              Format.fprintf ppf "%s" (Ezjsonm.to_string d)
            in
            M.assert_equal data' ~exp:data pp_data >>= fun _ ->
            M.no_next_step ()
      in
      M.({meth = `POST; uri;
          before_ts; after_ts;
          next_ts})
    in
    (name, [step0; step1])


  let case1 =
    let name = "invalide token" in
    let step0 =
      let uri  =
        let uri' = Uri.of_string arbiter_endp in
        Uri.with_path uri' "/routes-token"
      in
      let after_ts resp body =
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`OK s M.pp_status >>= fun r ->
        R.bind r (fun () ->
            M.put_env "x-api-key" body;
            R.ok ())
        |> return
      in
      M.({meth = `GET; uri;
          before_ts = empty_before_ts; after_ts;
          next_ts = no_next_step})
    in
    let step1 =
      let uri =
        let uri' = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
        Uri.with_path uri' "/export"
      in
      let before_ts () =
        let key =
          match M.get_env "x-api-key" with
          | None -> ""
          | Some k -> k
        in
        let h = Cohttp.Header.init_with "X-Api-Key" key in
        let b =
          make_body ""
          |> Cohttp_lwt_body.of_string
        in
        return (h, b)
      in
      let after_ts resp body =
        M.print_after_ts (resp, body) >>= fun _ ->
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`Unauthorized s M.pp_status
      in
      M.({meth = `POST; uri;
          before_ts; after_ts;
          next_ts = no_next_step})
    in
    (name, [step0; step1])


  let case2 =
    let name = "invalide request id" in
    let step0 =
      let uri  =
        let uri' = Uri.of_string arbiter_endp in
        Uri.with_path uri' "/token"
      in
      let after_ts resp body =
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`OK s M.pp_status >>= fun r ->
        R.bind r (fun () ->
            M.put_env "x-api-key" body;
            R.ok ())
        |> return
      in
      M.({meth = `GET; uri;
          before_ts = empty_before_ts; after_ts;
          next_ts = no_next_step})
    in
    let step1 =
      let uri =
        let uri' = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
        Uri.with_path uri' "/export"
      in
      let before_ts () =
        let key =
          match M.get_env "x-api-key" with
          | None -> ""
          | Some k -> k
        in
        let h = Cohttp.Header.init_with "X-Api-Key" key in
        let id =
          let id' = Uuidm.create `V4 in
          Uuidm.to_string id'
        in
        let b =
          make_body id
          |> Cohttp_lwt_body.of_string
        in
        return (h, b)
      in
      let after_ts resp body =
        M.print_after_ts (resp, body) >>= fun _ ->
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`Not_found s M.pp_status
      in
      M.({meth = `POST; uri;
          before_ts; after_ts;
          next_ts = no_next_step})
    in
    (name, [step0; step1])


  let case3 =
    let name = "invalide request format" in
    let step0 =
      let uri  =
        let uri' = Uri.of_string arbiter_endp in
        Uri.with_path uri' "/token"
      in
      let after_ts resp body =
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`OK s M.pp_status >>= fun r ->
        R.bind r (fun () ->
            M.put_env "x-api-key" body;
            R.ok ())
        |> return
      in
      M.({meth = `GET; uri;
          before_ts = empty_before_ts; after_ts;
          next_ts = no_next_step})
    in
    let step1 =
      let uri =
        let uri' = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
        Uri.with_path uri' "/export"
      in
      let before_ts () =
        let key =
          match M.get_env "x-api-key" with
          | None -> ""
          | Some k -> k
        in
        let h = Cohttp.Header.init_with "X-Api-Key" key in
        let id =
          let id' = Uuidm.create `V4 in
          Uuidm.to_string id'
        in
        let b =
          `O ["id",   `String id;
              "dest", `String ("http://127.0.0.1:" ^ local_echo_port);
              "data", `String (Ezjsonm.to_string data); ]
          |> Ezjsonm.to_string
          |> Cohttp_lwt_body.of_string
        in
        return (h, b)
      in
      let after_ts resp body =
        M.print_after_ts (resp, body) >>= fun _ ->
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`Not_found s M.pp_status
      in
      M.({meth = `POST; uri;
          before_ts; after_ts;
          next_ts = no_next_step})
    in
    (name, [step0; step1])


  let case4 =
    let name = "with long polling endpoint" in
    let step0 =
      let uri  =
        let uri' = Uri.of_string arbiter_endp in
        Uri.with_path uri' "/token"
      in
      let after_ts resp body =
        let s = Cohttp.Response.status resp in
        M.assert_equal ~exp:`OK s M.pp_status >>= fun r ->
        R.bind r (fun () ->
            M.put_env "x-api-key" body;
            R.ok ())
        |> return
      in
      M.({meth = `GET; uri;
          before_ts = empty_before_ts; after_ts;
          next_ts = no_next_step})
    in
    let rec step1 =
      let uri timeout =
        let uri' = Uri.of_string ("http://127.0.0.1:" ^ export_port) in
        let with_path = Uri.with_path uri' "/lp/export" in
        let with_param = Uri.add_query_param' with_path ("timeout", timeout) in
        with_param
      in
      let before_ts () =
        let key =
          match M.get_env "x-api-key" with
          | None -> ""
          | Some k -> k
        in
        let h = Cohttp.Header.init_with "X-Api-Key" key in
        let id =
          match M.get_env "id" with
          | None -> ""
          | Some id -> id
        in
        let uri = "http://127.0.0.1:" ^ local_echo_slow_port in
        let b =
          make_body ~uri id
          |> Cohttp_lwt_body.of_string
        in
        return (h, b)
      in
      let after_ts resp body =
        M.print_after_ts (resp, body) >>= fun _ ->
        get_field body "state" Ezjsonm.get_string >>= fun s ->

        let s = String.lowercase_ascii s in
        if not (s = "finished") then
          get_field body "id" Ezjsonm.get_string >>= fun id ->
          M.put_env "id" id;
          M.rm_env "ext_resp";
          return_ok ()
        else
        get_field body "ext_response" Ezjsonm.get_dict >>= fun ext_resp ->
        let v =
          `O ext_resp
          |> Ezjsonm.to_string
        in
        M.put_env "ext_resp" v;
        return_ok ()
      in
      let next_ts () =
        match M.get_env "ext_resp" with
        | None -> return_some step1
        | Some ext_resp ->
            let ext_resp = Ezjsonm.(
                from_string ext_resp
                |> value
                |> get_dict)
            in
            let data' =
              let open Ezjsonm in
              List.assoc "body" ext_resp
              |> get_string
              |> from_string
              |> value
              |> get_dict
              |> List.assoc "request"
            in
            let pp_data ppf d =
              let d = Ezjsonm.wrap d in
              Format.fprintf ppf "%s" (Ezjsonm.to_string d)
            in
            M.assert_equal data' ~exp:data pp_data >>= fun _ ->
            M.no_next_step ()
      in
      let uri = uri "" in
      M.({meth = `POST; uri;
          before_ts; after_ts;
          next_ts})
    in
    (name, [step0; step1])


  let client () =
    M.process_suit [case0; case1; case2; case3; case4]

end


module TC = Test_client''

let main () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Debug));
  match Lwt_unix.fork () with
  | 0 -> begin
      Unix.sleepf 2.5;
      try
        Lwt_main.run @@ TC.client ();
        Logs.info (fun m -> m "[client] OK!")
      with _ ->
        Logs.err (fun m -> m "[client] ERROR!");
        exit 1 end
  | pid ->
      let wait () =
        Lwt_unix.wait () >>= fun (cpid, status) ->
        assert (pid = cpid);
        if status = Unix.(WEXITED 0) then return_unit
        else Lwt.fail_with "client"
      in
      let t = wait () <?> server () in
      try
        Lwt_main.run t;
        Logs.info (fun m -> m "[server] OK!")
      with
      | Failure f when f = "client" ->
          Logs.info (fun m -> m "[server] OK!"); exit 1
      | _ ->
          Logs.err (fun m -> m "[server] ERROR!"); exit 1


let () = main ()
