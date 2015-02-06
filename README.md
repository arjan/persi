Persi: Erlang persistence layer
===============================

Abstraction layer for dealing with relational databases in
Erlang. Erlang versions from R15B01 and up are supported.

[![Build Status](https://travis-ci.org/arjan/persi.svg?branch=master)](https://travis-ci.org/arjan/persi)


Design goals
------------
* Simple, easy to learn API
* Support multiple database backends
* API is purely functional, it is not an ORM.
* Database schema versioning and migrations
* Aiming for 100% test coverage
* 100% typespec'd (dialyzer)


Quick start
-----------

Look in the eunit tests for the coverage of the complete Persi
API. All functions are exposed through the `persi` module.

Creating a table:

```
    -include_lib("persi/include/persi.hrl").
    
    Table = #persi_table{name=persons,
                         columns=
                             [
                              #persi_column{name=id, type=int},
                              #persi_column{name=name, type="varchar(50)", default="app", notnull=true}
                             ],
                         pk=[id]},
    persi:create_table(Table),
```

Insert some data:

```
    ok = persi:insert(persons, [{id, 1}, {name, "Piet"}]),
    ok = persi:insert(persons, [{id, 2}, {name, "Jan"}]),
```

Retrieve a single row on the PK:

    Piet = persi:select(persons, 1).

Or by another column:

    Piet = persi:select(persons, [{name, "Piet"}]).

Retrieve all rows:

    Persons = persi:rows(persons, "SELECT * FROM persons", []).


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
* pgsql: Disconnect inactive worker connections, like Zotonic does 


Naming
------
The name, `persi` is an abbreviation of the word
`persistence`. Because, after all, `persi`'s goal is to persist
data. At the same time, it's the abbreviation of Robin van Persie's
last name, a famous Dutch soccer player.



Driver Limitations
------------------

### PostgreSQL ###

* None

### MySQL ###

* No transaction support yet
* `default=current_timestamp` does not work on datetime columns

### sqlite ###

* `drop column` is not supported


Debugging
---------

Set the `log_queries` environment variable to enable query logging through the given logger function `Module:Function/2`:

    application:set_env(persi, log_queries, {io, format}).

Alternatively, you can set the `log_queries` variable to a callback function:

    application:set_env(persi, log_queries, fun(Query, Args) -> ... end).

