#!/usr/bin/env python
import grpc
import greet_pb2_grpc
from greet_pb2 import Text

with grpc.insecure_channel("localhost:50051") as channel:
    stub = greet_pb2_grpc.GreeterStub(channel)
    response = stub.SayHello(Text(txt=""))
    print(response)
