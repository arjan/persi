Persi: Erlang persistence layer
===============================

Abstraction layer for dealing with relational databases in Erlang.

[![Build Status](https://travis-ci.org/arjan/persi.svg?branch=master)](https://travis-ci.org/arjan/persi)


Design goals
------------
* Simple, easy to learn API
* Support multiple database backends
* API is purely functional, it is not an ORM.
* Database schema versioning and migrations
* Aiming for 100% test coverage
* 100% typespec'd (dialyzer)


Supported database drivers
--------------------------
* SQLite (`esqlite` project)
* MySQL (`emysql` with builtin connection pool)
* PostgreSQL (`epgsql` with `poolboy` connection pool)


Features
--------
* Metadata caching (table / schema info) in driver layer
* No result caching - caching should be done in application code
* Optionally allow for arbitrary key / values in tables (serialized in a blob column)
* Data migration from / to props column when adding/dropping columns
* Support for foreign key constraints


To do list
----------
* Create indices on tables
* Define API for querying and retrieving logical rows
* pgsql: Disconnect inactive worker connections, like Zotonic does
* Transaction support (hard, because `emysql` doesnt support it) 


Naming
------
The name, `persi` is an abbreviation of the word
`persistence`. Because, after all, `persi`'s goal is to persist
data. At the same time, it's the abbreviation of Robin van Persie's
last name, a famous Dutch soccer player.


