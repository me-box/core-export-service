FROM ocaml/opam:alpine

RUN sudo apk add libsodium-dev

RUN opam depext -y conf-m4.1
RUN opam pin add -y sodium https://github.com/dsheets/ocaml-sodium.git
RUN opam pin add -y macaroons https://github.com/nojb/ocaml-macaroons.git

