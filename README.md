# Cleff

Cleff provides a framework for extensible handling of computational effects.

The design is heavily inspired by the Eff programming language. This project
aims to provide a proof of concept implementation of all of the examples in
[Programming with Algebraic Effects and Handlers][1] by Bauer and Pretnar.

## Status

The basics of handler and computation co-routines are working.

Here's a simple non-deterministic choice effect:

```clojure
(handle [(value [x]
           [x])
         (decide []
           (concat (continue true) (continue false)))]
  (let [x (if (effect 'decide) 10 20)
        y (if (effect 'decide) 0 5)]
    (value (- x y))))
;;=> (10 5 20 15)
```

Lots left to do:

- Automatic insertion of "value" effects
- Effect instances
- Handlers for "finally"
- defprotocol-style defeffect
- Reusable/composable handlers
- Hierarchical handlers with forwarding
- Subroutines for coping with core.async's lexical IOC

## License

Copyright © 2013 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.


[1]: http://math.andrej.com/2012/03/08/programming-with-algebraic-effects-and-handlers/
