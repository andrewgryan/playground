# Assembler HTTP server

A blank canvas to practice x86_64 assembler.

## HTML templating

Replacing a value in a string on face value looks simple. In
assembler it requires identifiers to map to values.

```html
<h1>Hello, {{name}}!</h1>
```

When a request gets made for an endpoint, the `handle_request:` section
fans out the work to the appropriate sub-handler. Our program can
send a template, pattern and value to a handler.


## Memory

- 64-bit (8 bytes) pointer
- 64-bit (8 bytes) length
- 64-bit (8 bytes) pointer
- 64-bit (8 bytes) length
