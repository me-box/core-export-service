FROM ocaml/opam:alpine

RUN sudo apk add libsodium-dev libffi-dev

RUN opam depext -y conf-m4.1
RUN opam pin add -y sodium https://github.com/dsheets/ocaml-sodium.git
RUN opam pin add -y macaroons https://github.com/nojb/ocaml-macaroons.git

# tests use three ports
# while the service will be on 8080
EXPOSE 8080 8000 8888

RUN git clone https://github.com/sevenEng/databox-bridge.git \
  && cd databox-bridge \
  && git checkout dockerise \
  && opam pin add -n databox-export-service . \
  && opam install -t databox-export-service

ENTRYPOINT ["opam", "config", "exec", "--"]
CMD ["databox-export-service"]