## Goal

Build a CI server based on the [Simple Haskell Handbook](https://marcosampellegrini.com/simple-haskell-book)

## Features

- sandboxed builds in Docker containers
- a multi-node architecture with agents picking up jobs to work on • an http api to interact with the frontend and other nodes
- support for triggering builds with github webhooks

