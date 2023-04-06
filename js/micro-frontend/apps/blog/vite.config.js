import { defineConfig } from "vite";
import federation from "@originjs/vite-plugin-federation";
import solidPlugin from "vite-plugin-solid";

export default defineConfig({
  build: {
    target: "esnext",
  },
  plugins: [
    solidPlugin(),
    federation({
      name: "remote-app",
      filename: "remoteEntry.js",
      // Modules to expose
      exposes: {
        "./Button": "./src/Button.jsx",
      },
      shared: ["solid-js"],
    }),
  ],
});
