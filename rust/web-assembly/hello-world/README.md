# Build Rust
```
wasm-pack build --target web
```

# Start web server

Vite imports the `.js` and in turn `.wasm` in the `pkg` directory
created by `wasm-pack`.

```
npm run dev
```
