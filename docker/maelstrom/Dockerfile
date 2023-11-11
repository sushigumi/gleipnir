FROM alpine:3.18.4 as scratch-image

WORKDIR /tmp
ADD https://github.com/jepsen-io/maelstrom/releases/download/v0.2.3/maelstrom.tar.bz2 maelstrom.tar.bz2
RUN tar -xjf maelstrom.tar.bz2 && rm maelstrom.tar.bz2

FROM amazoncorretto:21.0.1-al2023-headless
WORKDIR /app
COPY --from=scratch-image /tmp/maelstrom/ /app/maelstrom/
RUN yum install -y git graphviz gnuplot

ENTRYPOINT [ "/app/maelstrom/maelstrom" ]

