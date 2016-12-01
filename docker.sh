#!/usr/bin/env bash

lein uberjar
docker build -t gklijs/snake .
docker run -d -p 3000:3000 --name snake_instance gklijs/snake