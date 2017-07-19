#! /bin/sh

sudo apk add libsodium-dev libffi-dev
sudo apk add m4 perl

opam init
eval `opam config env`
opam install -y depext
opam depext -y conf-gmp.1

opam pin add -n sodium https://github.com/me-box/ocaml-sodium.git#with_auth_hmac256
opam pin add -n opium https://github.com/me-box/opium.git#fix-ssl-option

opam pin add -y export-service .

mv /home/databox/.opam/system/bin/export-service /home/databox/service
rm -rf /home/databox/.opam

sudo apk del m4 perl
sudo apk del libffi-dev
