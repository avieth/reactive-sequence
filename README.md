# reactive-sequence

The excellent [reactive-banana](https://github.com/HeinrichApfelmus/reactive-banana)
is founded upon two types: the `Event` and the `Behavior`. The former allows us
to react to new values, but doesn't give us access to any notion of current or
most-recently-observed value; the latter gives us access to the current or
most-recently-observed value, but gives us no way to react to changes in that
value.

Consequently, neither the `Event` nor the `Behavior` is an applicative functor,
though each are functors. Informally, we can imagine a definition of `<*>` for
`Event`: `evF <*> efX` fires whenever either of its terms fires, and its value
is the latest `evF` applied to the latest `evX`, but there's no reasonable
definition of `pure :: t -> Event t`; this would have to be an event which
fires once and only once, at the beginning of time, but this can't be expressed
in reactive-banana. For `Behavior`, there's an obvious definition of `pure x`:
it takes the value `x` and never changes, as in `stepper x never`. But
`stepper` works only inside some `MonadMomet`.

In this package the type `Sequence` is defined. It's a value and an `Event`
giving changes to that value. Essentially, `Sequence t` is a formal
representation of `stepper :: MonadMoment m => t -> Event t -> m t`.
