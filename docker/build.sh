#!/usr/bin/env bash

TAG=${1:-latest}
FILE=$(find . -path \*$TAG/Dockerfile)

REPO=$(basename $(pwd))
IMAGE=ghcr.io/nationalparkservice/$REPO:$TAG

cd $(dirname $FILE)
docker build . --no-cache --rm -t $IMAGE
# docker build --no-cache --rm -t $IMAGE - < $FILE
