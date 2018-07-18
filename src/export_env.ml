let local_name = ref "export-service"
let local_port = ref "8080"

let secrets_dir = Fpath.v "/run/secrets/"


let local_name () =
  try Sys.getenv "DATABOX_LOCAL_NAME"
  with Not_found -> !local_name


let local_port () =
  try Sys.getenv "DATABOX_LOCAL_PORT"
  with Not_found -> !local_port


let arbiter_token () =
  let file_token = Fpath.add_seg secrets_dir "ARBITER_TOKEN" in
  (* Rresult.R.map B64.encode (Bos.OS.File.read file_token) *)
  (* Rresult.R.map B64.decode (Bos.OS.File.read file_token) *)
  Bos.OS.File.read file_token
  |> function
  | Ok token -> token
  | Error msg ->
      Logs.warn (fun m -> m "[env] failed to read ARBITER_TOKEN using default value\n[env] %a" Rresult.R.pp_msg msg);
      "secret"

let arbiter_public_key () =
  let file_key = Fpath.add_seg secrets_dir "ZMQ_PUBLIC_KEY" in
  (* Rresult.R.map B64.encode (Bos.OS.File.read file_key) *)
  (* Rresult.R.map B64.decode (Bos.OS.File.read file_token) *)
  Bos.OS.File.read file_key
  |> function
  | Ok key -> key
  | Error msg ->
      Logs.warn (fun m -> m "[env] failed to read ZMQ_PUBLIC_KEY using default value\n[env] %a" Rresult.R.pp_msg msg);
      "vl6wu0A@XP?}Or/&BR#LSxn>A+}L)p44/W[wXL3<"

let arbiter_uri () = "tcp://arbiter:4444"

(* deprecated
let init_certs () =
  let open Rresult.R in
  let file_pem = Fpath.add_seg secrets_dir "DATABOX_EXPORT_SERVICE_PEM.json" in
  Bos.OS.File.read file_pem >>= fun rjson ->
  (try
    let obj = Ezjsonm.from_string rjson in
    let dict = Ezjsonm.get_dict obj in
    let key = Ezjsonm.get_string (List.assoc "clientprivate" dict) in
    let cert = Ezjsonm.get_string (List.assoc "clientcert" dict) in
    ok (cert, key)
  with _ -> error_msg "json parsing/format error")

  >>= fun (env_cert, env_key) -> begin
    let open Bos.OS in
    Dir.user () >>= fun user ->
    let cert_dir =
      Fpath.add_seg user "certs"
      |> Fpath.to_dir_path
    in
    Dir.create cert_dir >>= fun _ ->

    let file_cert = Fpath.add_seg cert_dir "public.cert"
    and file_key  = Fpath.add_seg cert_dir "private.key" in
    File.delete file_cert >>= fun () ->
    File.delete file_key  >>= fun () ->
    File.write file_cert env_cert >>= fun () ->
    File.write file_key env_key   >>= fun () ->
    ok (file_cert, file_key)
  end
*)

let init_https () =
  let file_cert = Fpath.add_seg secrets_dir "DATABOX.pem" in
  let file_key  = Fpath.add_seg secrets_dir "DATABOX.pem" in
  Rresult.R.ok (file_cert, file_key)
