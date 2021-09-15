#!/usr/bin/env bash

TAG=${1:-latest}
FILE=$(find . -path \*$TAG/Dockerfile)

REPO=$(basename $(pwd))
IMAGE=ghcr.io/nationalparkservice/$REPO:$TAG

BUILD_CONTEXT=$(dirname $FILE)

cp assets/Latin-Modern-Roman-fontfacekit.zip $BUILD_CONTEXT
cd $BUILD_CONTEXT
docker build . --no-cache --rm -t $IMAGE
rm Latin-Modern-Roman-fontfacekit.zip
