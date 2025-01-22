# `transducers`

An implementation of reducers and transducers in Haskell.

## Overview

The core idea of transducers is to implement core functionality like `map` or `filter` or `compareLength` only once and be able to use it on any container-like type.
This is done with `Reducers`, that are basically fancy `fold`s, they're records with folding step function, initial state, initial accumulator, and a finalizer.
The step function can also short-circuit the reduction.

This is a very simple yet powerful implementation, there are no type classes or fancy types in general.
The `transducers-core` library has no dependencies.
Utilities for non-`base` data structures are in separate packages:
- `transducers-vector`

There is one difference between this implementation and others - reducers apart from storing initial state also store initial accumulator value.
This just seems more intuitive for me, almost always there is only one reasonable initial value and while for `sum` it may be obvious to start with `0` but it starts to get tricky for `compareLength`.

## Credits & References

- [Transducers in Clojure](https://clojure.org/reference/transducers)
- [Implementation of transducers in Haskell](https://github.com/hypirion/haskell-transducers)
- [Blog post on stateful transducers in Haskell](https://hypirion.com/musings/haskell-transducers)
- [Blog posst on non-stateful non-short-circuiting transducers in Haskell](https://conscientiousprogrammer.com/blog/2014/08/07/understanding-cloure-transducers-through-types/)

## License

Copyright (C) 2025 Tomasz Maciosowski (t4ccer)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
