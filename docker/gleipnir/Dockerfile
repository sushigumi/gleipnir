ARG MAELSTROM_IMAGE

FROM haskell:9.4.7 AS build-image
WORKDIR /app
COPY ./gleipnir.cabal gleipnir.cabal
RUN cabal update \
    && cabal build --only-dependencies

COPY ./src/ src/
COPY ./executables executables/
RUN cabal build gleipnir-echo gleipnir-unique-ids \
    && mv $(cabal exec which gleipnir-echo) /app/gleipnir-echo \
    && mv $(cabal exec which gleipnir-unique-ids) /app/gleipnir-unique-ids

FROM ${MAELSTROM_IMAGE}
COPY --from=build-image /app/gleipnir* /app/

