import type { Component } from 'solid-js';
import { createResource, createEffect } from 'solid-js';
import { createClient } from '@urql/core';

import logo from './logo.svg';
import styles from './App.module.css';

const client = createClient({
  url: 'http://localhost:3000/graphql'
})
const [hello] = createResource(() => client.query(`{hello}`).toPromise())

const App: Component = () => {
  createEffect(() => {
    console.log(hello())
  })
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
