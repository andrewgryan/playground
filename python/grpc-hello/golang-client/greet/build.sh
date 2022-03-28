#!/bin/bash
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    --go_opt Mgreet.proto=example/users/golang-client/greet \
    --proto_path=../.. \
    greet.proto
