FROM ocaml/opam:alpine-3.6_ocaml-4.04.2 as BUILDER

WORKDIR /core-export-service
ADD core-export-service.export core-export-service.export

RUN sudo apk update && sudo apk add alpine-sdk bash ncurses-dev m4 perl gmp-dev zlib-dev libsodium-dev libffi-dev libressl-dev zeromq-dev &&\
#    opam remote add git https://github.com/ocaml/opam-repository.git &&\
    opam pin add -n opium https://github.com/me-box/opium.git#term-argv &&\
    opam pin add -n sodium https://github.com/me-box/ocaml-sodium.git#with_auth_hmac256 &&\
    opam install -y logs cmdliner uuidm fpath rresult bos ppx_bitstring opium websocket-lwt cohttp depyt sodium macaroons lwt-zmq reason
#    opam switch import core-export-service.export

ADD . .
RUN sudo chown opam: -R . && opam config exec -- jbuilder build src/service.exe


FROM alpine:3.6

WORKDIR /core-export-service
RUN apk update && apk add libsodium gmp libzmq
COPY --from=BUILDER /core-export-service/_build/default/src/service.exe service

EXPOSE 8080

LABEL databox.type="export-service"

RUN addgroup -S databox && adduser -S -g databox databox
USER databox

CMD ["./service", "-v"]
