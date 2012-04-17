# RAGE, Results And Graphing Engine

## Description

A new framework being developed by the performance team to analyse the data
collected, and display it as dynamic graphs on a webpage.

## Overview

Test runs (jobs) are run on specific machines using specific builds of the
product. Each test job can contain multiple test cases, each of which can
contain multiple scales of measure (SOMs). Each such SOM can product multiple
measurements.

Since the framework is fairly generic, its database schema is not trivial:

![UML-like database schema](https://github.com/perf101/rage/raw/master/doc/uml-schema.png)

* **tiny\_urls**: A simple table for tiny URL support.
* **test\_cases**: Test case fully-qualified names and descriptions.
* **soms**: SOM (scale of measure) definitions, where each SOM is part of a
  specific test case.
* **builds**: Product build definitions. Each build is associated with a
  (source-control) branch, build number, and potentially a custom build tag.
* **branch_order**: Ordering among different branches. RAGE is currently
  configured to ignore this table, ordering branches alphabetically.
* **jobs**: A list of test runs (jobs). Each job is associated with a product
  build, and can optionally remember the original test command.
* **tc\_config\_&lt;tc_fqn&gt;**: A table for each test case type. This table
  enumerates the relevant configuration option permutations for a specific
  test case.
* **som\_config\_&lt;som_id&gt;**: An optional table for each SOM in case a SOM is
  associated with configuration options not captured by its test case.
* **machines**: Test machine definitions.
* **tc\_machines**: A table that links test case runs to specific machines.
* **measurements**: The table of measurements. Each measurements is associated
  with a test job, test case configuration, SOM, and (optionally) SOM
  configuration. The field `result_id` is used to allow multiple measurements
  for each unique permutation of the stated fields.

We also provide the SQL to generate the main tables:
\[[blob](https://github.com/perf101/rage/blob/master/sql/schema.sql)\],
\[[raw](https://github.com/perf101/rage/raw/master/sql/schema.sql)\].

## Importing data into RAGE

To make the task of importing data into RAGE database easier, we have created
a tool that will hopefully be of help:
[RAGE Importer](https://github.com/perf101/rage-importer).

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
settings for accessing the database can be changed in `OMakefile`.

Installation of the program as a web service (`omake install`) simply copies
the generated distribution (`omake distro`) into a web-server documents
directory (`/var/www` by default). The installation currently assumes that flot
files can be found in a sub-directory of the install directory.

Any errors will be shown in the web-server's error log (e.g.
`/var/log/apache2/error.log`).

### Citrix-internal instructions

A Citrix-internal machine that satisfies the above conditions is called `perf`.
Make sure that your `~/.bashrc` includes the following lines:

    export PATH=/usr/local/perf/js/bin:$PATH
    export MANPATH=/usr/local/perf/js/man:$MANPATH
    
This makes sure that you are using the correct versions of OCaml tools.
