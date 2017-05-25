let local_name = ref "databox-export-service"
let local_port = ref "8080"

let arbiter_endpoint = ref "https://databox-arbiter:8080"
let arbiter_key = ref ""

let secrets_dir = Fpath.v "/run/secrets/"


let local_name () =
  try Sys.getenv "DATABOX_LOCAL_NAME"
  with Not_found -> !local_name


let local_port () =
  try Sys.getenv "DATABOX_LOCAL_PORT"
  with Not_found -> !local_port


let arbiter_endp () =
  try Sys.getenv "DATABOX_ARBITER_ENDPOINT"
  with Not_found -> !arbiter_endpoint


let arbiter_token () =
  if !arbiter_key <> "" then !arbiter_key
  else begin
    let file_token = Fpath.add_seg secrets_dir "DATABOX_EXPORT_SERVICE_KEY" in
    Rresult.R.map B64.decode (Bos.OS.File.read file_token)
    |> function
    | Ok token ->
        arbiter_key := token;
        token
    | Error msg ->
        Logs.err (fun m -> m "[env] ARBITER_TOKEN %a" Rresult.R.pp_msg msg);
        ""
  end


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
