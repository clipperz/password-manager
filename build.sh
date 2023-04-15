#!/bin/sh

imageName="clipperz-app"
version=$1
commit=$2

docker build -t "${imageName}:${version}" -t "${imageName}:${commit}" --build-arg CURRENT_COMMIT_ARG=${commit} .
