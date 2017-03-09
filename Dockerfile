FROM ocaml/opam:alpine

RUN sudo apk add libsodium-dev libffi-dev

RUN opam depext -y conf-m4.1
RUN opam pin add -n sodium https://github.com/dsheets/ocaml-sodium.git
RUN opam pin add -n macaroons https://github.com/nojb/ocaml-macaroons.git
RUN opam pin add -n depyt https://github.com/sevenEng/depyt.git#fix-opam

# tests use three ports
# while the service will be on 8080
EXPOSE 8080 8000 8888 8008

RUN git clone https://github.com/me-box/databox-export-service.git \
  && cd databox-export-service \
  && opam pin add -n databox-export-service . \
  && opam install --deps-only databox-export-service \
  && eval `opam config env` \
  && ocaml pkg/pkg.ml build --tests true \
  && ocaml pkg/pkg.ml test \
  && opam install databox-export-service

LABEL databox.type="export-service"

ENTRYPOINT ["opam", "config", "exec", "--"]
CMD ["databox-export-service"]
