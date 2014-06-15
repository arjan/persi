Persi: Erlang persistence layer
===============================

Abstraction layer for dealing with relational databases in Erlang.

[![Build Status](https://travis-ci.org/arjan/persi.svg?branch=master)](https://travis-ci.org/arjan/persi)


Design goals
------------

* Simple, easy to learn API
* API is purely functional, it is not an ORM.
* Database schema versioning and migrations
* Aiming for 100% test coverage
* No result caching - caching should be done in application code
* Aiming for 100% test coverage
* 100% typespec (dialyzer)

Features
--------
* Metadata caching (table / schema info) in driver layer
* Optionally allow for arbitrary key / values in tables (serialized in a blob column)
* Support for foreign key constraints


To do list
----------
* Data migration from / to props column when adding/dropping columns
* Implement multiple database backends (currently only SQLite)
* Connection pooling (for applicable backends)


Naming
------

The name, `persi` is an abbreviation of the word
`persistence`. Because, after all, `persi`'s goal is to persist
data. At the same time, it's the abbreviation of Robin van Persie's
last name, a famous Dutch soccer player.


