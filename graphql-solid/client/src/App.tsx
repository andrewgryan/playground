import type { Component } from "solid-js";
import { createResource, createEffect } from "solid-js";
import { createClient } from "@urql/core";

import logo from "./logo.svg";
import styles from "./App.module.css";

const client = createClient({
  url: "http://localhost:3000/graphql",
});

const QUERY = `
  query RollDice($dice: Int!, $sides: Int) {
    rollDice(numDice: $dice, numSides: $sides)  
  }
`;
const [hello] = createResource(() =>
  client.query(QUERY, { dice: 3, sides: 6 }).toPromise()
);

const App: Component = () => {
  createEffect(() => {
    console.log(hello());
  });
  return (
    <div class={styles.App}>
      <header class={styles.header}>
        <img src={logo} class={styles.logo} alt="logo" />
        <p>
          Edit <code>src/App.tsx</code> and save to reload.
        </p>
        <a
          class={styles.link}
          href="https://github.com/solidjs/solid"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn Solid
        </a>
      </header>
    </div>
  );
};

export default App;
