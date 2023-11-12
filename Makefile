MAELSTROM_VERSION = "0.2.3"

APP_NAME = "gleipnir"
IMAGE_NAME = "${APP_NAME}"
VERSION = "0.0.1"

.PHONY: maelstrom
maelstrom:
	@echo "+ $@"
	@docker build -t maelstrom:${MAELSTROM_VERSION} --build-arg VERSION=${MAELSTROM_VERSION} -f docker/maelstrom/Dockerfile .
	@echo "Done."

.PHONY: gleipnir
gleipnir: 
	@echo "+ $@"
	@docker build -t ${IMAGE_NAME}:${VERSION} --build-arg MAELSTROM_IMAGE=maelstrom:${MAELSTROM_VERSION} -f ./docker/gleipnir/Dockerfile .
	@echo "Done."
	@docker images --format '{{.Repository}}:{{.Tag}}\t\t Built: {{.CreatedSince}}\t\tSize: {{.Size}}' | grep "${IMAGE_NAME}:${VERSION}"

run-echo: gleipnir
	@echo "+ $@"
	@docker run --rm ${IMAGE_NAME}:${VERSION} test -w echo --bin /app/gleipnir-echo --nodes n1 --time-limit 10 --log-stderr

run-unique-ids: gleipnir
	@echo "+ $@"
	@docker run --rm ${IMAGE_NAME}:${VERSION} test -w unique-ids --bin /app/gleipnir-unique-ids --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition

run-broadcast: gleipnir
	@echo "+ $@"
	@docker run --rm ${IMAGE_NAME}:${VERSION} test -w broadcast --bin /app/gleipnir-broadcast --node-count 1 --time-limit 20 --rate 10

