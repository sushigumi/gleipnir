ARG MAELSTROM_IMAGE

FROM haskell:9.4.7 AS build-image
WORKDIR /app
COPY ./gleipnir.cabal gleipnir.cabal
RUN cabal update \
    && cabal build --only-dependencies

COPY ./src/ src/
RUN cabal build gleipnir \
    && mv $(cabal exec which gleipnir) /app/gleipnir

FROM ${MAELSTROM_IMAGE}
COPY --from=build-image /app/gleipnir /app/gleipnir

