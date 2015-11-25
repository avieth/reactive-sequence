# reactive-sequence

The excellent [reactive-banana](https://github.com/HeinrichApfelmus/reactive-banana)
is founded on two types: the `Event` and the `Behavior`. The former allows us
to react to new values, but doesn't give us access to any notion of current or
most-recently-observed value; the latter gives us access to the current or
most-recently-observed value, but gives us no way to react to changes in that
value.

The `Sequence` type is a bit of both: it can be like an `Event`, like a
`Behavior`, or like both! Always showing a most-recently-observed value *and*
a way to observe changes. It's a `Functor`, and it also ships with
`Applicative`- and `Alternative`-like combinators (the actual classes are too
restrictive), and a generalized notion of event switching (`switchE` from
`Reactive.Banana.Combinators`).

## `Sequence` is parameterized by two functors

To derive the notion of `Sequence`, notice that if we want both an `Event` and
a `Behavior` corresponding to that event, it's enough to give `(t, Event t)`
and pass this data through `stepper :: t -> Event t -> MomentIO (Behavior t)`.
But this type is too small. It can express a `Behavior`, but not an `Event`.
If we throw an arbitrary type of kind `* -> *` in front of the first part of
the tuple, to get `(f t, Event t)`, then setting `f ~ Const ()` gives an
`Event t`, and `f ~ Identity` gives the previous type `(t, Event t)`. Let's
throw in another parameter, to get `(f t, Event (g t))` because why not? That's
essentially it:

```Haskell
-- The actual type is different, but this is the idea.
type Sequence (f :: * -> *) (g :: * -> *) (t :: *) = (f t, Event (g t))

-- Just a value. There can be no more occurrences because we can't make a
-- Const Void.
type SValue = Sequence Identity (Const Void)

-- Just an event. The initial value has type Const (), of which there is only
-- one inhabitant.
type SEvent = Sequence (Const ()) Identity

-- The mutant hybrid of Event and Behavior. There is an initial value and
-- an event giving new values.
type SBehavior = Sequence Identity Identity
```

## `Sequence` is like an `Applicative`

Imagine if there were an `Applicative` instance for `Event`. Informally,
combining two `Event`s using `<*>` gives an `Event` which fires with the
new value whenever either `Event` fires, and both `Event`s have fired at least
once, so that we can remember the most recent value and use it to recompute
the function.
But then what about `pure`? It would have to give an `Event` which somehow has
always fired, but never fires again, so that `evf <*> pure x` fires when and
only when `evf` fires and always passes `x` to its function. Seems hopeless,
unless we introduce the notion of some kind of "initial event" which fires
precisely once to indicate the beginning of time. Or, we could use `Sequence`.

There is a function `always :: Applicative f => t -> Sequence f g t`.
Specializing to `f ~ Identity`, `g ~ Const ()` we get
`always :: t -> SBehavior t`. It's just what it sounds like: it has the
value you give, and it never changes. This serves as `pure` in our
kind-of-`Applicative`.

To derive an alaogue of `<*>`, we must consider what it means to use this on
various combinations of `SEvent` and `SBehavior`:

  - `SEvent (s -> t) -> SEvent s -> SEvent t` fires whenever either
    event fires, and both have fired at least once.
  - `SEvent (s -> t) -> SBehavior s -> SEvent t` fires whenever the
    event fires.
  - `SBehavior (s -> t) -> SEvent s -> SEvent t` fires whenever the
    event fires.
  - `SBehavior (s -> t) -> SBehavior s -> SBehavior t` fires whenever either
    behavior changes.

Notice how the type of the output `Sequence` depends upon the types of
the input `Sequence`s. This is intuitive: if one of the terms is an `SEvent`
then we cannot possibly recover an `SBehavior`, because we're missing an
initial value; we simply cannot compute the function until that `SEvent` fires
for the first time to deliver its value. Unfortunately, since the types change,
we can't call this an `Applicative`, so instead we just give `<%>` in place
of `<*>`.

## `Sequence` is like an `Alternative`

The function `unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a`
allows us to derive an `Event` which fires whenever either of two `Event`s
fires, with the most recent value, subject to a disambiguating function in
case of simultaneous occurrences. The event `never :: Event t` serves as an
identity under `unionWith`. This resembles an `Alternative`. In fact, if
we have an `Event` of some semigroup, and if `Event` were an `Applicative`,
we could make an `Alternative`:

```Haskell
-- Event would have to always contain a semigroup.
instance Alternative Event where
    empty = never
    <|> = unionWith (<>)
```

We have some `Alternative`-like infrastructure for `Sequence` as well, and just
as for its `Applicative`-like functions, it works to combine `Sequences` of
different parameters. The players are:

```Haskell
-- A Sequence containing no information. It holds () and never fires.
nothing :: Sequence (Const ()) (Const Void) t

-- Union two sequences, firing whenever either of them fires, and using their
-- Semigroup instance to disambiguate simultaneous occurrences.
--
-- Actually, there are more constraints, and f3 and g3 are determined by
-- f1, f2, g1, and g2.
(<||>) :: Semigroup s => Sequence f1 g2 s -> Sequence f2 g2 s -> Sequence f3 g3 s
```

The interaction of `<||>` with the type parameters is dual to that of `<%>`.
Whereas `<%>` will give an `SEvent` if either of its parameters are `SEvent`,
`<||>` will give an `SBehavior` if either of its parameters are `SBehavior`.
That's because `<||>` needs only one initial value to give its own initial
value, whereas `<%>` would require both: the function, and the point at which
to evaluate it.

## `Sequence` can be switched

It's often useful to dynamically switch events via

```Haskell
switchE :: Event (Event t) -> Event t`
switchB :: Behavior a -> Event (Behavior a) -> Behavior a 

-- But what about something like this?
switchEB :: Behavior (Event t) -> Event t
```

That last one, `switchEB`, does not exist as far as I know. Intuitively, it
should means that we always have an `Event t`, but it vary over time, and
whenever it changes, the resulting `Event t` should ignore the previous
one and only fire when the latest one fires. Maybe this can be done using
the `reactive-banana` combinators, but regardless, it can also be done using
`Sequence`.

```Haskell
-- The type actually has constraints, and f3 and g3 are determined by
-- f1, f2, g1, and g2.
switch :: (t -> t -> t) -> Sequence f1 g1 (Sequence f2 g2 s) -> Sequence f3 g3 s
```

Sepcializing to `SBehavior (SEvent t)`, we find that switching this gives
an `SEvent t`, which is in analogy to `switchEB`.
