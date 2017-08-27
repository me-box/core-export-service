FROM ocaml/opam:alpine-3.4_ocaml-4.04.2 as BUILDER

WORKDIR /export-service
ADD . .

RUN sudo apk add m4 perl libsodium-dev libffi-dev gmp-dev
RUN opam pin add -n sodium https://github.com/me-box/ocaml-sodium.git#with_auth_hmac256
RUN opam pin add -n opium https://github.com/me-box/opium.git#fix-ssl-option
RUN opam pin add -y export-service /export-service


FROM alpine:3.4

WORKDIR /core-export-service
RUN apk update && apk add libsodium-dev gmp-dev
COPY --from=BUILDER /home/opam/.opam/4.04.2/bin/export-service service

EXPOSE 8080

LABEL databox.type="export-service"

CMD ["./service"]
