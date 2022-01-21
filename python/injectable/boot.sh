#!/bin/bash
black --target-version py310 *.py
npm run build
bokeh serve --show .
