#!/bin/sh

app_version=$1
commit=$2

docker build -t "clipperz-app:${app_version}" -t "clipperz-app:${commit}" --build-arg CURRENT_COMMIT_ARG=${commit} .
