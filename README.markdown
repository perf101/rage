# RAGE, Results And Graphing Engine

## Description

A new framework being developed by the performance team to analyse the data
collected, and display it as dynamic graphs on a webpage.

## Tools used

* [postgresql](http://www.postgresql.org/)
* [postgresql-ocaml](http://www.ocaml.info/home/ocaml_sources.html#postgresql-ocaml)
* [flot](http://code.google.com/p/flot/)

## Development instructions

The Makefile expects that the system `make` is run on has all the required
tools installed, has a running database system with exact database and user
setup. The internal machine that satisfies these conditions is called `perf`.
