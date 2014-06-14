Persi: Erlang persistence layer
===============================

Abstraction layer for dealing with relational databases in Erlang.

Design goals
------------

* Simple, easy to learn API
* API is purely functional, it is not an ORM.
* Database schema versioning and migrations
* Multiple database backends
* Connection pooling (for applicable backends)
* No result caching - caching should be done in application code
* Aiming for 100% test coverage


Usage
-----


Naming
------

The name, `persi` is an abbreviation of the word
`persistence`. Because, after all, `persi`'s goal is to persist
data. At the same time, it's the abbreviation of Robin van Persie's
last name, a famous Dutch soccer player.
