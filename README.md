databox-export-service - export service for databox platform
-------------------------------------------------------------------------------
%%VERSION%%

databox-export-service is distributed under the MIT license.

Homepage: https://github.com/me-box/databox-export-service

## Installation

databox-export-service can be installed with `opam`:

	opam pin -n add databox-export-service https://github.com/me-box/databox-export-service.git
    opam install databox-bridge


Or build your own docker image:

    docker build -t <image name> .

The docker container solution is recommended, as there are extra system dependencies and local package pins to make it work for the `opam` installation. All of this has been taken care of by steps in Dockerfile.


## API

This service only exposes one endpoint:

    Method   : POST
	URL      : /export
	Parameter: JSON object that has the format {id: <request id>, dest: <destination url>, data: <export data>}
	Response : JSON object with the format {id: <request id>, state: <request state>, response: <response>}

When making a query to this endpoint, make sure there is a `X-Api-Key` header included, it contains an arbiter-minted macaroon.

The service will use `id` field in request parameter to differentiate between a newly submitted export request and a polling action about some previously submitted one. If the field is left as an empty string, it will be treated as a new export request, and the export service will put it in a queue and generate its unique id, and include the id in the reponse JSON object. The `data` field will be parsed as a stringified JSON object.

The client could poll the state of its request by including the service provided id. The `state` is of string type, and could be one of `"Pending"`, `"Processing"`, and `"Finished"`. If the state is `"Finished"`, the `response` field will be a raw JSON body as `{status: <status code>, body: <response body>}`.

