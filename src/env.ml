let local_name = ref "databox-export-bridge"
let local_port = ref "8080"

let arbiter_endpoint = ref "https://databox-arbiter:8080"
let arbiter_key = ref None


let local_name () =
  try Sys.getenv "DATABOX_LOCAL_NAME"
  with Not_found -> !local_name


let local_port () =
  try Sys.getenv "DATABOX_LOCAL_PORT"
  with Not_found -> !local_port


let arbiter_endp () =
  try Sys.getenv "DATABOX_ARBITER_ENDPOINT"
  with Not_found -> !arbiter_endpoint


let arbiter_key () =
  try Some (Sys.getenv "ARBITER_KEY")
  with Not_found -> !arbiter_key

