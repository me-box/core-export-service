#!/bin/bash

ocaml server.ml &
server_pid=$!

./../_build/bin/service.native -v -s perfsecret &
service_pid=$!

sleep 1

exp_m=$(cat export_m)
dmp_m=$(cat dump_m)

body=$(cat export.data) && echo "body to post: " $body

for i in {1..10}
do
    curl -X POST -d $body -H "x-api-key:$exp_m" -w "\n" http://127.0.0.1:8080/export
done

curl -H "x-api-key:$dmp_m" -w "\n" http://127.0.0.1:8080/dump

echo "kill mock server ..." && kill $server_pid
echo "kill export service ..." && kill $service_pid
