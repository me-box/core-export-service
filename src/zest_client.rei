type options = {
  request_endpoint : string,
  uri_path : string,
  identity : string,
  server_key : string,
  arbiter_token : string,
};

let get_secret : options => v::bool? => unit => Lwt.t string;
