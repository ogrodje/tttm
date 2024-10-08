#!/usr/bin/env bash
set -ex
DOCKER_IMAGE=${DOCKER_IMAGE:-"ghcr.io/ogrodje/tttm-randy:latest"}
export DOCKER_COMPOSE_FLAGS=${DOCKER_COMPOSE_FLAGS:="-f docker-compose.dev.yml"}

./bin/tttm-wrap.sh $@
