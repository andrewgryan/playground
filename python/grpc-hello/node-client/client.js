const PROTO_PATH = "../greet.proto";
const grpc = require("@grpc/grpc-js");
const protoLoader = require("@grpc/proto-loader");
const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
    keepCase: true,
    longs: String,
    enums: String,
    defaults: true,
    oneofs: true,
});
const protoDescriptor = grpc.loadPackageDefinition(packageDefinition);
const stub = new protoDescriptor.Greeter(
    "localhost:50051",
    grpc.credentials.createInsecure()
);

// Call Python SayHello() function
stub.sayHello({ txt: "" }, function (err, text) {
    console.log(text);
});
