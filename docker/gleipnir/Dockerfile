ARG MAELSTROM_IMAGE

FROM haskell:9.4.7 AS build-image
WORKDIR /app
COPY ./gleipnir.cabal gleipnir.cabal
RUN cabal update \
    && cabal build --only-dependencies

COPY ./src/ src/
COPY ./executables executables/
COPY ./docker/gleipnir/runner.sh runner.sh
RUN cabal build gleipnir-echo gleipnir-unique-ids gleipnir-broadcast \
    && mv $(cabal exec which gleipnir-echo) /app/gleipnir-echo \
    && mv $(cabal exec which gleipnir-unique-ids) /app/gleipnir-unique-ids \
    && mv $(cabal exec which gleipnir-broadcast) /app/gleipnir-broadcast \
    && chmod 777 ./runner.sh

FROM ${MAELSTROM_IMAGE}
COPY --from=build-image /app/gleipnir* /app/runner.sh /app/.

ENTRYPOINT ["/app/runner.sh"]

