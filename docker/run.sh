#!/usr/bin/env bash

TAG=${1:-latest}
ENTRYPOINT=${2:-/init}
PORT=${3:-8787}

REPO=$(basename $(pwd))
IMAGE=ghcr.io/nationalparkservice/$REPO:$TAG

if [[ "$(docker images -q $IMAGE 2> /dev/null)" == "" ]]; then
  echo "Image not found, building from recipe...."
  ./$(find . -path \*build.sh) $TAG
fi

echo "http://localhost:$PORT/ (with usr and pwd 'bayes')"

rm -rf kitematic/ .rstudio/ rstudio/
docker run -it --rm \
		--name rstudio-bayes-$PORT \
		-v "$(pwd)":/home/bayes \
		-w /home/bayes \
		-e USER="bayes" \
		-e PASSWORD="bayes" \
		-p $PORT:8787 \
		$IMAGE \
    $ENTRYPOINT

rm -rf kitematic/ .rstudio/ rstudio/
