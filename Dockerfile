FROM ocaml/opam:alpine

RUN sudo apk add libsodium-dev libffi-dev

RUN opam depext -y conf-m4.1
RUN opam depext -y conf-gmp.1
RUN opam depext -y conf-perl.1

RUN opam pin add -n sodium https://github.com/dsheets/ocaml-sodium.git
RUN opam pin add -n macaroons https://github.com/sevenEng/ocaml-macaroons.git#use-url-safe-alphabet
RUN opam pin add -n depyt https://github.com/sevenEng/depyt.git#fix-opam
RUN opam pin add -n opium https://github.com/sevenEng/opium.git#fix-ssl-option

WORKDIR /home/opam

ADD . databox-export-service

RUN cd databox-export-service \
 && opam pin add -y databox-export-service .

EXPOSE 8080

LABEL databox.type="export-service"

CMD ["databox-export-service"]
