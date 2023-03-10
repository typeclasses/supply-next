## The interface

`Next` is a [supply-chain] interface for possibly-finite enumeration. It is
defined in `Next.Interface.Type` as follows:

```haskell
data Next item result =
    (result ~ Step item) => Next
```

```haskell
data Step item = Item item | End
```

It is envisioned some of the utilities in this package may usefully extend to
more complex supply-chain interfaces, in which "next" appears as one of many
operations in a larger interface. The utilities we provide are therefore
polymorphic where possible. The class that permits this polymorphism, defined
in `Next.Interface.Class`, is called `TerminableStream`.

```haskell
class TerminableStream item interface | interface -> item where
    next :: interface (Step item)
```


## Type aliases

A *producer* is a vendor whose downward-facing interface is a stream; it
provides items. A *consumer* is a job whose upward-facing interface is a stream;
it consumes items. A *pipe* is a vendor whose up and down interfaces are both
streams, possibly of differing item types; it both consumes and provides items.

```haskell
type Producer action item         = Vendor (Const Void) (Next item)  action
type Pipe     action item1 item2  = Vendor (Next item1) (Next item2) action
type Consumer action item product = Job    (Next item) action product
```

The "plus" variants of these aliases are more general in their upstream
interfaces and have an additional parameter to describe exactly what the
upstream interface is.

```haskell
type ProducerPlus up action item         =                              Vendor up (Next item)  action
type PipePlus     up action item1 item2  = TerminableStream item1 up => Vendor up (Next item2) action
type ConsumerPlus up action item product = TerminableStream item  up => Job    up action product
```


## The Stream type

`Stream` is a bonus feature, simply a newtype for `Producer`. This can help
abstract away from the `supply-chain` library to simplify compiler feedback,
and it may be easier to use if you do not need the full generality of the
`supply-chain` library.

```haskell
newtype Stream action item =
    Stream{ producer :: Producer action item }
```

`Stream` has some class instances, but otherwise this library offers no
utilities for working with `Stream`. Instead, work with the `Producer` type
and wrap/unwrap the `Stream` constructor as needed.


## Other libraries

`supply-next` is one contribution to the [supply-chain] library ecosystem.

A consumer can be constructed from an `EffectfulFold`; this type is from the
[gambler] library, which contains a number of common ways to construct folds.

See also [foldl], whose `FoldM` type is the same as `EffectfulFold`, for more
ideas about what kinds of things you can express as folds.

The terms *producer*, *pipe*, and *consumer* are borrowed from [pipes]. The
`Stream` type is also inspired by the inclusion of the `ListT` bonus type in
pipes.

If you are only using `Stream` and do not need the general facilities of
[supply-chain] at all, the `ListT` type in [list-transformer] may be a more
appropriate choice. Consider also that a type as simple as `IO (Maybe item)`
can also suffice to represent an effectful stream.


  [supply-chain]:     https://hackage.haskell.org/package/supply-chain
  [gambler]:          https://hackage.haskell.org/package/gambler
  [foldl]:            https://hackage.haskell.org/package/foldl
  [pipes]:            https://hackage.haskell.org/package/pipes
  [list-transformer]: https://hackage.haskell.org/package/list-transformer
