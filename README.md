# Lecture: Fun with type-level programming 

Code shown in the CS141 lecture titled "Fun with type-level programming". This repository contains an implementation of the infamous Witter coursework, but in Haskell. The project is split into three packages:

## witter-shared 

The `witter-shared` package implements all of Witter's data types and application logic, such as querying different stores for data in various ways. The data types have suitable type class instances for converting them to/from JSON and from CSV.

This package also describes the Witter API as a type using `servant`.

## witter-server 

The `witter-server` package implements a REST API for Witter using `servant-server`.

## witter-client 

The `witter-client` package implements a client for the Witter REST API using `servant-client`.
