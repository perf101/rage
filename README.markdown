# RAGE, Results And Graphing Engine

## Description

A new framework being developed by the performance team to analyse the data
collected, and display it as dynamic graphs on a webpage.

## Tools used

* [OCaml](http://caml.inria.fr/ocaml/index.en.html), 3.12.1
* [OMake](http://omake.metaprl.org), 0.9.8.6
* [postgresql](http://www.postgresql.org/), 8.4

## Libraries used

* [Core](http://www.janestreet.com/ocaml/doc/core/index.html), 107.01
* [postgresql-ocaml](http://www.ocaml.info/home/ocaml_sources.html#postgresql-ocaml), 1.16.0
* [flot](http://code.google.com/p/flot/)

## Development instructions

The program can be built on a system where OCaml, OMake, Core, and
postgresql-ocaml are installed.

To run the program (`omake run`), a postgresql database must be running. The
default settings (can be changed in `OMakefile`) are:
* username: `www-data`;
* password; `perf`; and,
* database: `perf`.

Installation of the program as a web service (`omake install`) simply copies
the generated distribution (`omake distro`) into a web-server documents
directory (`/var/www` by default). The installation currently assumes that flot
files can be found in a sub-directory of the install directory.

### Citrix-internal instructions

A Citrix-internal machine that satisfies the above conditions is called `perf`.
Make sure that your `~/.bashrc` includes the following lines:

    export PATH=/usr/local/perf/js/bin:$PATH
    export MANPATH=/usr/local/perf/js/man:$MANPATH
    
This makes sure that you are using the correct versions of OCaml tools.
