#! /bin/sh

sudo apk add libsodium-dev libffi-dev

opam init
eval `opam config env`
opam install -y depext
opam depext -y conf-m4.1 conf-gmp.1 conf-perl.1

opam pin add -n sodium https://github.com/sevenEng/ocaml-sodium.git#with_auth_hmac256
opam pin add -n macaroons https://github.com/nojb/ocaml-macaroons.git
opam pin add -n depyt https://github.com/sevenEng/depyt.git#fix-opam
opam pin add -n opium https://github.com/sevenEng/opium.git#fix-ssl-option

opam pin add -y databox-export-service .

mv /home/databox/.opam/system/bin/databox-export-service /home/databox/export-service
rm -rf /home/databox/.opam
