#!/usr/bin/env python
from concurrent import futures
import grpc
from greet_pb2 import Text
import greet_pb2
import greet_pb2_grpc


class GreeterServicer(greet_pb2_grpc.GreeterServicer):
    def SayHello(self, request, context):
        return Text(txt="Hello, World!")


def serve(port: str = "[::]:50051"):
    """gRPC boiler-plate"""
    server = grpc.server(futures.ThreadPoolExecutor())
    greet_pb2_grpc.add_GreeterServicer_to_server(GreeterServicer(), server)
    server.add_insecure_port(port)
    server.start()
    server.wait_for_termination()


if __name__ == "__main__":
    serve()
