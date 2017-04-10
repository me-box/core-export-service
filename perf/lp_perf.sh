#!/bin/bash

ocaml server.ml &
server_pid=$!

./../_build/bin/service.native -vv --secret perfsecret --long-polling &
service_pid=$!

sleep 1

ocaml perf_lp.ml

echo "kill mock server ..." && kill $server_pid
echo "kill export service ..." && kill $service_pid

