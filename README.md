# training-fp

Training material in Scala on functors, monads and applicative functors.


## Functors
Functors define the `map` operation.

Satisfy the laws:
* Identity: `fa map identity == fa`.
* Composition: `fa map (g(f(_))) == fa map f map g`.


## Monads
Monads define the operations `flatMap` and `pure`.

Satisfy the laws:
* Left identity: `pure(a) flatMap f == f(a)`
* Right identity: `m flatMap pure == m`
* Associativity: `m.flatMap(f).flatMap.(g) == m.flatMap(a => f(a).flatMap(g))`

Monads are Functors by defining `map` based on `flatMap` and `pure`. Therefore they also satisfy both functor laws:
* Identity: `m map identity == m`
* Composition: `m map (g(f(_))) == m map f map g`

`map2` can be defined based on the monad operations as:
```
def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
  ma flatMap (a => mb map (b => f(a, b)))
```

`traverse` and `sequence` can be defined based on the monad operations as:
```
def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(pure(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

def sequence[A](fas: List[F[A]]): F[List[A]] =
  traverse[F[A], A](fas)(identity)
```
(Note that traverse's definition above applies f to the elements of the original list `as` right-to-left. Alternative, 
more complex definitions might apply f to the elements of `as` left-to-right. Cats is an example of such definition.)

`replicateM` and `product` can also be derived for a Monad:
```
def replicateM(n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n, fa))
def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
```
(Note how `replicateM` replicates the whole monad, not only the _element_. I.e. an implementation such as
`def badReplicateM(n: Int, fa: F[A]): F[List[A]] = map(fa)(List.fill(n)(_))` would not be correct.)

As well as the Kleisli composition:
```
def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
``` 

This latter allows an alternative expression of the monad laws:
* Left identity: `compose(pure, f) == f`
* Right identity: `compose(f, pure) == f`
* Associativity: `compose(compose(f, g), h) == compose(f, compose(g, h))`


## Applicative Functor
Applicative functors can be defined based on the primitive operations `pure` and `map2`. The following
operations can be then derived:
* `map[A, B](fa: F[A])(op: A => B): F[B]`
* `traverse[A, B](as: List[A])(op: A => F[B]): F[List[B]]`
* `sequence[A](fas: List[F[A]]): F[List[A]]`
* `replicateM[A](n: Int, fa: F[A]): F[List[A]]`
* `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`
* `apply[A, B](fop: F[A => B])(fa: F[A]): F[B]`

Alternatively, we can define applicative functors based on the primitive operations `pure` and `apply`.
Then the other operations listed above, including `map2`, can be derived from the primitive ones.


## Bibliography
* [Noel Welsh and Dave Gurnell, Scala with Cats](https://underscore.io/books/scala-with-cats/)
* [Paul Chiusano and Rúnar Bjarnason, Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala)

