# mrm ![example workflow](https://github.com/tozarin/mrm/actions/workflows/act.yml/badge.svg) [![Coverage Status](https://coveralls.io/repos/github/Tozarin/mrm/badge.svg?branch=dev)](https://coveralls.io/github/Tozarin/mrm?branch=dev)
Module Relational Mapper library for OCaml. Maps OCaml first-class modules and user's code-querys in SQL's data and querys.
Uses [Caqti library](https://github.com/paurkedal/ocaml-caqti) to SQL-things

## Usage
Include the Uuid module and add the mrm directive to the first-class module of interest

```ocaml
module type Foo = sig
  include Uuid

  val foo : string
end
[@@deriving mrm]
```

This will generate the module Foo_DB with next functions
```ocaml
module Foo_DB : sig
    type 'mainmods conns = ((module Foo), 'mainmods) Mrm.Db.Connection.conn

    val connect_inited : (module Caqti_blocking.CONNECTION) -> [< `RW | `RO ] conns
    val connect_not_inited : (module Caqti_blocking.CONNECTION) -> [ `NOTINITED ] conns

    val get_new_uuid : [< `RW | `RO ] conns -> ((int * [< `RW | `RO ] conns), 'err) Result.t
    val new_foo : string -> [< `RW | `RO ] conns -> (((module Foo) * [< `RW | `RO ] conns), 'err) Result.t

    val init : [< `NOTINITED | `DROP ] conns -> ([ `RW ] conns, 'err) Result.t
    val drop : [ `RW ] conns -> ([ `DROP ] conns, 'err) Result.t
    val add_deep : (module Foo) -> [ `RW ] conns -> ([ `RW ] conns, 'err) Result.t
    val add : (module Foo) -> [ `RW ] conns -> ([ `RW ] conns, 'err) Result.t
    val delete : (module Foo) -> [ `RW ] conns -> ([ `RW ] conns, 'err) Result.t
    val select_by_uuid : int -> [< `RW | `RO ] conns -> (((module Foo) * [< `RW | `RO ] conns), 'err) Result.t
    val select_all : [< `RW | `RO ] conns -> (((module Foo) list * [< `RW | `RO ] conns), 'err) Result.t
    val commit : (module Foo) list -> [< `RW ] conns -> ([< `RW | `RO ] conns, 'err) Result.t
    val migrate : [< `RW | `RO ] conns -> [< `RW ] conns -> ([< `RW | `RO ] conns, 'err) Result.t
end
```

## Functions
- connect_inited - creates the connection-typed value to allready inited table
- connect_not_inited - creates the connection-typed value to not inited table
- get_new_uuid - creates the int-value to ident modules
- new_foo - creates the new module
- init - initiates related table
- drop - drops related table
- add_deep - adds the module in table and his submodules to their tables
- add - add the module in table
- delete - deletes the module
- select_by_uuid - returns the module with given uuid
- select_all - returns all modules from table
- commit - updates given modules in table
- migrate - copys all modules from one bd to another

## Supported types
Library supports next types:
- int, string, bool, float
- first-class module

## Supported databases
- PostgreSQL
- SQLite
- MariaDB