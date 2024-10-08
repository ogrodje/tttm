#!/usr/bin/env bash
set -ex
TTTM_HOME=${TTTM_HOME:=$(pwd)}
DOCKER_COMPOSE_FLAGS=${DOCKER_COMPOSE_FLAGS:=""}

if [[ -z "${TTTM_HOME}" ]]; then
  echo "TTTM_HOME environment variable is not set!" && exit 255
fi

# shellcheck disable=SC2068
cd "$TTTM_HOME" &&
  docker compose \
    -f docker-compose.yml ${DOCKER_COMPOSE_FLAGS} \
    --project-name tttm \
    --project-directory . $@
