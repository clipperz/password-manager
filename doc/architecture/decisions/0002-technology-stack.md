# 2. Technology Stack

Date: 2023-08-24

## Status

Accepted

## Context

We need to decide with which technologies this application will be developed. 
<!-- With the old code base of Clipperz being very hard to work with, we want to use tools that will allow the code to be tidy and manageable -->

## Decision

### Backend

We will use the [Scala](https://www.scala-lang.org) language, with the [ZIO](https://zio.dev) framework and ecosystem.

### Frontend

We will use [Purescript](https://www.purescript.org); the [Concur](https://github.com/purescript-concur) library will be used to build the UI, the [Affjax](https://github.com/purescript-contrib/purescript-affjax) library to communicate with the backend and the [subtlecrypto](https://github.com/clipperz/purescript-subtlecrypto/tree/clipperz) library that provides the basic cryptographic functions, creating a wrapping for Purescript of the ones implemented in browsers.

## Consequences

The functional approach that the chosen languages and frameworks use will be a hard entry level as it differs from the "standard" way of writing software. On the other hand this approach allows the compiler to catch a majority of the errors and reduces the run-time errors almost to zero.
