#!/usr/bin/env bash

# Action code

Rscript $1
time=$(date)
echo "::set-output name=time::$time"
