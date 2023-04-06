import { defineConfig } from "vite";
import federation from "@originjs/vite-plugin-federation";
import solidPlugin from "vite-plugin-solid";

export default defineConfig({
  cors: {
    origin: true
  },
  plugins: [
    solidPlugin(),
    federation({
      name: "host-app",
      remotes: {
        remote: "https://experimental-module-federation.s3.eu-west-2.amazonaws.com/assets/remoteEntry.js",
      },
      shared: ["solid-js"],
    }),
  ],
});
