let local_name = ref "databox-export-bridge"
let local_port = ref "8080"

let arbiter_endpoint = ref "https://databox-arbiter:8080"
let arbiter_key = ref ""


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
  try Sys.getenv "ARBITER_TOKEN"
  with Not_found -> !arbiter_key


let cert_path = ref ""
let key_path  = ref ""


let cert_path () =
  try Sys.getenv "HTTPS_SERVER_CERT"
  with Not_found -> !cert_path


let key_path () =
  try Sys.getenv "HTTPS_SERVER_PRIVATE_KEY"
  with Not_found -> !key_path
