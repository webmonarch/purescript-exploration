hello-world
===========

``` purescript
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
```

Breakdown:

`forall e.`

`data Foo = Foo | Bar String`: `Foo` is a `tagged union` data type with two constructors, `Foo` and `Bar`. Used for pattern matching.

