var express = require("express");
var { graphqlHTTP } = require("express-graphql");
var { buildSchema } = require("graphql");

// Construct a schema, using GraphQL schema language
var schema = buildSchema(`
  type Mutation {
    setMessage(message: String): String
  }

  type RandomDie {
    roll(numRolls: Int!): [Int!]!
    rollOnce: Int
  }

  type Query {
    getDie(numSides: Int): RandomDie
    getMessage: String
  }
`);

// The root provides a resolver function for each API endpoint
class RandomDie {
  constructor(numSides) {
    this.numSides = numSides
  }
  rollOnce() {
      return 1 + Math.floor(Math.random() * (this.numSides || 6));
  }
  roll({numRolls}) {
    var output = [];
    for (var i = 0; i < numRolls; i++) {
      output.push(this.rollOnce());
    }
    return output;
  }
}

let fakeDatabase = {}

var root = {
  setMessage: ({ message }) => {
    fakeDatabase.message = message
    return message
  },
  getMessage: () => {
    return fakeDatabase.message
  },
  getDie: ({ numSides }) => {
    return new RandomDie(numSides || 6)
  }
};

var app = express();
app.use(
  "/graphql",
  graphqlHTTP({
    schema: schema,
    rootValue: root,
    graphiql: true,
  })
);
app.listen(4000);
console.log("Running a GraphQL API server at http://localhost:4000/graphql");
