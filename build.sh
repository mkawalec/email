#!/usr/bin/env bash
stack build --stack-yaml stack.ghcjs.yaml
cp $(stack path --stack-yaml stack.ghcjs.yaml --local-install-root)/bin/frontend.jsexe/all.js frontend/bundle.js
