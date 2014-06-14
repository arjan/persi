Persi: Erlang persistence layer
===============================

Abstraction layer for dealing with relational databases in Erlang.

[![Build Status](https://travis-ci.org/arjan/persi.svg?branch=master)](https://travis-ci.org/arjan/persi)


Design goals
------------

* Simple, easy to learn API
* API is purely functional, it is not an ORM.
* Database schema versioning and migrations
* No result caching - caching should be done in application code
* Metadata caching (table / schema info) in driver layer


To do list
----------
* Aiming for 100% test coverage
* Foreign key tests
* Optionally allow for arbitrary data in tables (serialized in a blob column)
* Implement multiple database backends (currently only SQLite)
* Connection pooling (for applicable backends)


Naming
------

The name, `persi` is an abbreviation of the word
`persistence`. Because, after all, `persi`'s goal is to persist
data. At the same time, it's the abbreviation of Robin van Persie's
last name, a famous Dutch soccer player.


