#!/bin/bash
docker build -t stocker/db1 .

docker run --name db1 -d stocker/db1
