#!/bin/bash

WD=/home/ql272/workspace/databox-export-service

ocaml server.ml &
server_pid=$!

$WD/_build/bin/service.native -v --secret perfsecret --long-polling &
service_pid=$!

sleep 1

if [ "$#" -gt 1 ]; then
    ocaml perf_lp.ml $1 $2
else
    ocaml perf_lp.ml
fi


echo "kill mock server ..." && kill $server_pid
echo "kill export service ..." && kill $service_pid

