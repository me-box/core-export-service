open Cmdliner

let endpoint =
  let typs = [
    `Polling, Arg.info ["polling"] ~doc:"polling version of the service, default if absent";
    `Long_polling, Arg.info ["long-polling"] ~doc:"long-polling version of the service";
    `Websocket, Arg.info ["websocket"] ~doc:"websocket version of the service"
  ] in
  Arg.(value & vflag `Polling & typs)


let secret =
  let doc = "if unset, retrieve from arbiter" in
  Arg.(value & opt (some string) None & info ~doc  ["s"; "secret"])


let port =
  let doc = "if set, this will overwrite the value from env variable" in
  Arg.(value & opt (some int) None & info ~doc ["p"; "port"])


let service endpoint secret port () =
  let main =
    match endpoint with
    | `Websocket -> Export_ws.ws ?secret ?port ()
    | `Polling -> Export_polling.polling ?secret ?port ()
    | `Long_polling -> Export_polling.polling ~lp:true ?secret ?port ()
  in
  Lwt_main.run main


let setup_log level =
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


let setup_log =
  Term.(const setup_log $ Logs_cli.level ())


let () =
  let info = Term.info ~doc:"databox export service" "service" in
  let term = Term.(const service $ endpoint $ secret $ port $ setup_log) in
  match Term.eval (term, info) with
  | `Ok () | `Version | `Help -> exit 0
  | `Error _ -> exit 1
