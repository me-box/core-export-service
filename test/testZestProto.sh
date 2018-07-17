NET=arbiter-export-net
ZEST_IMAGE_VERSION="jptmoore/zest:v0.0.8"

docker kill arbiter
docker kill export-service
docker network rm ${NET}
docker network create ${NET}


echo 'start arbiter'
docker run -d --name arbiter --network ${NET} --rm jptmoore/arbiter \
    /app/zest/server.exe \
    --secret-key-file example-server-key \
    --token-key-file example-token-key \
    --enable-logging

echo 'register export-service'
docker run --network ${NET} -it --rm ${ZEST_IMAGE_VERSION} \
    /app/zest/client.exe \
    --server-key 'vl6wu0A@XP?}Or/&BR#LSxn>A+}L)p44/W[wXL3<' \
    --request-endpoint tcp://arbiter:4444 --path '/cm/upsert-container-info' \
    --mode post \
    --payload "{\"name\": \"export-service\", \"type\": \"app\", \"key\": \"secret\"}" \
    --token secret

echo 'start export-service'
docker run --name export-service --network ${NET} -it --rm \
    -v $(pwd)/certs:/run/secrets \
    export-service:latest