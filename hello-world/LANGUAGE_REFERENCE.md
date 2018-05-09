Language Reference Notes
========================

https://github.com/purescript/documentation/tree/master/language

# Types

https://github.com/purescript/documentation/blob/master/language/Types.md

## Type System

Defined types:

Primitive Types: Int, Number, String, Char, Boolean
Arrays
Records
Tagged Unions
Newtypes
Functions
Polymorphic Types
Constrained Types
Type Synonyms
Rows

## Tagged Unions

https://en.wikipedia.org/wiki/Tagged_union
https://en.wikipedia.org/wiki/Algebraic_data_type

Example: `data Foo = Foo | Bar String`

`Foo` is a "tagged union" with two constructors `Foo` and `Bar`.

## Newtypes

Example: `newtype Percentage = Percentage Number`

Is like a data type but is restricted to a single constructor with a single argument. At runtime, the representation of it is the same as it's underlying type.

Newtypes are distinct to the type checker and can be assigned their own type class instances.

## Polymorphic Types

https://wiki.haskell.org/Polymorphism

Example: `forall t0. t0 -> t0`

## Row Polymorphism

Example: `forall r. { foo :: Int, bar :: Int | r } -> Int`

`r` type variable has kind `# Type` - it represents a row of types and can be instantiated with any row of named types

## Rank N Types

https://wiki.haskell.org/Rank-N_types

```
poly :: (forall a. a -> a) -> Boolean
poly f = (f 0 < 1) == f true
```

## Rows

Example:

``` purescript
-- closed row
( name :: String, age :: Number )

-- open row
( name :: String, age :: Number | r )
```

A row of types represents an unordered collection of named types (with duplicates). Rows are not of kind `Type`: they have kind `# k` for some kind `k`, and so rows cannot exist as a value. 

## Type Synonyms

You can define a synonym for a type using the `type` keyword. Type synonym can contain a type argument but cannot be partially applied. Type synonyms can apply to records, function, and rows.

Examples

``` purescript
-- Create an alias for a record with two fields
type Foo = { foo :: Number, bar :: Number }

-- Create an alias for a polymorphic record with the same shape
type Bar a = { foo :: a, bar :: a }
-- Foo is now equivalent to Bar Number

-- Create an alias for a complex function type
type Baz = Number -> Number -> Bar Number

-- Define type synonyms to help write complex Effect rows
-- This will accept further Effects to be added to the row
type RandomConsoleEffects eff = ( random :: RANDOM, console :: CONSOLE | eff )
-- This limits the Effects to just RANDOM and CONSOLE
type RandomConsoleEffect = RandomConsoleEffects ()
```

## Type Annotations

Most types can be inferred (except Rank N Types and Constrained Types). You can use `::` to optionally either as a declaration or after an expression.

## Kind System

The kind system defines the following kinds:

* `Type`, the kind of types.
* Arrow kinds `k1 -> k2`
* Row kinds `# k`
* User-defined kinds, such as `Control.Monad.Eff.Effect`, the kind of effects.

## Row Kinds

https://github.com/purescript/documentation/blob/master/language/Types.md#row-kinds

# Syntax

https://github.com/purescript/documentation/blob/master/language/Syntax.md

## Whitespace

The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

## Comments

Single line comment: `-- this is a comment`

Multiline comment:

``` purescript
{-
  Comment
  continued comment
-}
```

Docs comments `-- | Docs about a thing here`

```
-- | `bool` performs case analysis for the `Boolean` data type, like an `if` statement.
bool :: forall a. Boolean -> a -> a -> a
bool true x _ = x
bool false _ x = x
```

## Top-level declarations

Values at the top level of a module are defined by providing a name followed by an equals sign and then the value to associate. Functions are defined the same. Functions with pattern matching can be defined multiple times.

```
-- value
one = 1

-- function
add x y = x + y

-- pattern matching
isEmpty [] = true
isEmpty _ = false

-- guards
isEmptyAlt xs | length xs == 0 = true
isEmptyAlt _ = false

-- with type signature
multiply :: Number -> Number -> Number
multiply x y = x * y
```

## String Literals

`"` single quote, can span multiple lines with:

```
"hello \
\World"
```

`"""` if line breaks are required or you don't want to escape special character sequences (like in RegEx)

## Inline Function (Lambdas)

`\a b -> a + b`

## Record Literal

Use `_` in records to create a function that returns a record: `{ foo: _, bar: _ }`

`_.propertyName` equiv to `\rec -> rec.propertyName`

## Record Updates

https://github.com/purescript/documentation/blob/master/language/Syntax.md#record-updates

`rec { key1 = value1, ..., keyN = valueN, nestedKey { subKey = value, ... } }`

Wildcard can be used for a partially applied update:
`rec { foo = _ }` is equiv to `\foo -> rec { foo = foo }`

Wildcards can be used in the object position:
`_ { foo = 1 }` is equiv to `\rec -> rec { foo = 1 }`

## Binary Operators

Operators are defined by defining an alias for an existing binary function (`a -> b -> c`)

Example: `infixr 5 append as <>`

Parts:

* The associativity: either `infixl`, `infixr`, or `infix`.
    * https://github.com/purescript/documentation/blob/master/language/Syntax.md#associativity
* The precedence: an integer, between 0 and 9. Here, it is 5.
    * https://github.com/purescript/documentation/blob/master/language/Syntax.md#precedence
* The function to alias: here, `append`
* The operator: here, `<>`.

## Operators as values

Operators can be used as values when surrounded by parentheses:

`and = (&&)`

Operators can be partially applied when surrounded by parentheses and wildcard: `half = (_ / 2)`

Functions can be used as infix operators with ``` ` ``` (backticks): ```test = 10 `foo` 20```

## Case Expressions

Pattern matching and supports guards:

``` purescript
-- multiple values
f :: Maybe Boolean -> Either Boolean Boolean -> String
f a b = case a, b of
  Just true, Right true -> "Both true"
  Just true, Left _ -> "Just is true"
  Nothing, Right true -> "Right is true"
  _, _ -> "Both are false"
f (Just true) (Right true)

-- using guards
f :: Either Int Unit -> String
f x = case x of
  Left x | x == 0 -> "Left zero"
         | x < 0 -> "Left negative"
         | otherwise -> "Left positive"
  Right _ -> "Right"
```

## If/Then/Else

`if 2 > 1 then "okay" else "oops"`

`else` is always required

## Let / Where Bindings

`let` / `in` introduces a collection of local declarations.

```
factorial :: Int -> Int
factorial =
  let
    go :: Int -> Int -> Int
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)
  in
    go 1
```

`where` can introduce a local declaration at the end of a value declaration

```
factorial :: Int -> Int
factorial = go 1
  where
  go :: Int -> Int -> Int
  go acc 1 = acc
  go acc n = go (acc * n) (n - 1)
```

## Do Notation

The `do` keyword introduces simple syntactic sugar for monadic expressions. When using `do` notation, there must be a corresponding instance of the `Monad` type class for the return type.

```
maybeSum :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum a b = do
  n <- a
  m <- b
  let result = n + m
  pure result
```

Statements can have the following form:

* `a <- x` which desugars to `x >>= \a -> ...`
* `x` which desugars to `x >>= \_ -> ...` or just `x` if this is the last statement.
* A let binding `let a = x`. Note the lack of the `in` keyword.

This desugars to:

```
maybeSum a b =
  a >>= \n ->
    b >>= \m ->
      let result = n + m
      in pure result
```

Note: `(>>=)` is the `bind` function for the `Bind` type

# Type Classes

Types appearing in class instances must be of the form `String`, `Number`, `Boolean`, or `C t1` ... tn where `C` is a type constructor (including `->` and `t_i` are types of the same form).

Example for `Show` type class

``` purescript
class Show a where
  show :: a -> String

instance showString :: Show String where
  show s = s

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showArray :: (Show a) => Show (Array a) where
  show xs = "[" <> joinWith ", " (map show xs) <> "]"

example = show [true, false]
```

Type classes can be multi-parameter: https://leanpub.com/purescript/read#leanpub-auto-multi-parameter-type-classes

Type classes cannot be "orphaned", they need to be defined in the same module as either the class or the type.

## Superclass

Superclass implications can be indicated in a class declaration with a backwards fat arrow `<=`

Example: Defining `MonadFail` (a subclass of `Monad`)

``` purs
class (Monad m) <= MonadFail m where
  fail :: forall a. String -> m a
```

## Functional Dependencies

https://leanpub.com/purescript/read#leanpub-auto-functional-dependencies

## Type Class Deriving

Use `derive instance` to automatically derive a type class instance.

```
newtype Person = Person { name :: String, age :: Int }

derive instance eqPerson :: Eq Person
derive instance ordPerson :: Ord Person
```

* Data.Generic (class Generic)
* Data.Generic.Rep (class Generic)
* Data.Eq (class Eq)
* Data.Ord (class Ord)
* Data.Functor (class Functor)
* Data.Newtype (class Newtype)

# Pattern Matching

Pattern matching deconstructs a value to bring zero or more expressions into scope. Pattern matches are introduced with the `case` keyword.

## Wildcard Patterns

## Literal Patterns

## Variable Patterns

## Array Patterns

## Constructor patterns

## Record Patterns

## Nested Patterns

## Named Patterns

Named patterns bring additional names into scope when using nested patterns. Any pattern can be named by using the `@` symbol:

``` purescript
f a@[_, _] = a
f _ = []
```

Here, in the first pattern, any array with exactly two elements will be matched and bound to the variable a.

## Guards

Guards are used to impose additional constraints inside a pattern using boolean-valued expressions, and are introduced with a pipe after the pattern:

```
evens :: List Int -> Int
evens Nil = 0
evens (Cons x xs) | x `mod` 2 == 0 = 1 + evens xs
evens (Cons _ xs) = evens xs
```

## Pattern Guards

Pattern guards extend guards with pattern matching, notated with a left arrow. 

For example, we can apply a function fn to an argument x, succeeding only if fn returns Just y for some y, binding y at the same time:

```
bar x | Just y <- fn x = ... -- x and y are both in scope here
```

# Modules

Module definition with:

```
module B where
  
import A
```

Specify what specifically to import. Type constructors look like `TypeC(..)`

```
module B where

import A (runFoo, (.~), Foo(..), Bar(Bar))
```

Can exclude specific symbols from an import: `import A hiding (runFoo)`

# Reference

* `class Functor` defines `map`
* `<$>` is an alias for `map`
* `map` rules
  * `map id` = `id`
  * `map (g <<< g)` = `map f <<< map g`
* `<#>` alias for `mapFlipped`
* `<@>` alias for `flap`
* `void`
* `voidRight` alias `<$`
* `voidLeft` alias `$>`
