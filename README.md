# *Haskell Notes*

### Weak Head Normal Form

* While simplifying an expression, a Normal Form (NF) is a form where no further simplification can be performed; the expressions form reached its final stage and cannot be simplified any more.

* `Haskell` tries to evaluate an input expression into a NF.

* Certain expressions do not have a Normal Form; for instance the `haskell` definition `undefined = undefined` when simplified goes into an infinite loop as there is no final stage for it.

* The expressions evaluation rule has a significant effect on simplification. As for the simplification to reach a final stage or stop at some point depends on the evaluation rule.
for instance `const x = \y -> x` returns a constant function by ignoring it's argument `y` thus returning `x` always.
Strict languages say "*the argument expression shall be evaluated before the function call, and the called function shall receive the obtained value as its parameter*".
This is called `Call By Value`. With `CBV` an expression like `const 0 undefined` would loop forever as it will try to evaluate the arguments forever.
Lazy languages like `haskell` say "*the called function shall receive the unevaluated argument expression as its parameter*".
This is called `Call By Need`. With `CBN` an expression like `const 0 undefined` can be as follows
    ```haskell
    const 0 undefined = (\x -> 0) undefined
    				  = 0
    ```
* The called function can decide if it needs the value of the argument. For instance, if the value is a list, the function can decide how many elements it needs to have.

* In `haskell` yf the code is written as one long line (with the {;}s) then we always simplify at the first possible position. That position is called the `Head` and the end result would `Head NF` or `HBF`

* Functional programming languages stop as soon as the overall shape of the whole expression is not a call, but something like `\f -> f x` and evaluating the argument `x` does not make sense any more, because we do not know the function `f` using it anyway. This is called `Weak HNF` or `WHNF`.

#### some examples

#### Haskell Type Class syntax

```haskell
data Choice = Definitely
            | Possibly
            | NoWay
            deriving ( Eq, Ord, Enum, Bounded, Show, Read )

class Equals a where
  isEqual :: a -> a -> Bool

instance Equals Choice where
  isEqual Definitely Definitely = True
  isEqual Possibly   Possibly   = True
  isEqual NoWay      NoWay      = True
  isEqual _          _          = False

instance (Equals a) => Equals [a] where
  isEqual (a:as) (b:bs) = isEqual a b &&
                          isEqual as bs
  isEqual as     bs     = null as && null bs

instance Eq Choice where
  Definitely == Definitely = True
  Possibly   == Possibly   = True
  NoWay      == NoWay      = True
  _          == _          = False
```

>*fibonacci numbers using pairs of lazy list*

```haskell
fibs :: Num a -> [a]
fibs = map snd $ iterate (\(x, y) -> (y, x + y)) (0, 1)

-- λ> take 10 fibs
--     [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
```

>*a smiple triangle*

```haskell
λ> mapM_ print $ map (flip replicate '#') [1..5]
"#"
"##"
"###"
"####"
"#####"
```

#### Folding

`haskell` provides folding of a data structure from either left or right. The standard `haskell` library defines the right fold as similar to the following

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc xs = case xs of
                      []       -> acc
                      (y : ys) -> f y (foldr f acc ys)

```
The `foldr` function takes a binary function and an identity value along with a list and reduces the list. It can be represented as a binary tree as follows for a list `[x1, x2, x3]` and a function `fn` with an initial value as `u`

```haskell

foldr fn u [x1, x2, ..., xn] = x1 `fn` (x2 `fn` (...(xn `fn` u)...))

                fn
                / \
              x1    fn
                   /  \
                  x2    fn
                        / \
                      x3     u
```

The `foldl` function does essentiall the same except that it folds or reduces the list to the left. For a function `fn` and initial identity element `u` over the list `[x1,x2,x3]`, we can represent foldl as follows

```haskell

foldl fn u [x1, x2, ..., xn] = (...((u `fn` x1) `fn` x2) `fn` ...) `fn` xn

                  fn
                  / \
                fn    x1
               / \
            fn     x2
           / \
         u     x3
```

Its defined as below in the standard `haskell` library

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc xs = case xs of
                      []       -> acc
                      (y : ys) -> foldl f (f acc y) ys
```

We can have the below identity between the left fold and right fold

```haskell
foldr op u xs == foldl (flip op) u (reverse xs)
```

#### function composition (.)

> Mathematically composition of functions is denoted by a `o`.
> So, `(f o g)(x) = f(g(x))`

The same can be represented in `haskell` using the `(.)` operator defined as below.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

### `Functors`
-----------------------------

A `Functor` type class is for the things, which can be mapped over. In normal terms we can say that a `Functor` applies a normal function to a wrapped value to return a wrapped value.
Here is how a `Functor` class is defined in the standard `haskell` library.

```haskell
class Functor (f :: * -> *) where
	fmap :: (a -> b) -> f a -> f b
```

> A `Functor` class takes a type constructor that takes one type. All
> instances of the `Functor` class should satisfy the below two laws:

```haskell
fmap id == id
fmap (f . g) == fmap f . fmap g
```

*All the instances of `Functor` for `lists`, `Maybe` and `IO` satisfy the above laws.*

> Here is how popular  structures like `lists`, `Maybe`, `Either` and
> `->` are instances of the `Functor` type class

```haskell
-- list as a functor instance

instance Functor [] where
	fmap = map

-- or

instance Functor [] where
    fmap f []       = []
    fmap f (x : xs) = f x : fmap f xs

-- Maybe as a functor instance

instance Functor Maybe where
	fmap f (Just x) = Just (f x)
	fmap f (Nothing) = Nothing

-- Either as a functor

instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x)   = Left x

-- IO as a functor

instance Functor IO where
	fmap f x = do
		result <- x
		return (f resullt)

-- (->) as a functor

instance Functor ((->) r) where
	fmap f g = (\x -> f (g x))
```


*With (`->`) instance of `Functor` we can have the below interesting points*

>`fmap :: (a -> b) -> f a -> f b`

*replace all f's with (`->`) r*

>`fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`

*`((->) r a)` and `((->) r b)` can be written as `r -> a` and `r -> b`*

>`fmap :: (a -> b) -> (r -> a) -> (r -> b)`

The above relation is nothing more than a function composition and can
be re-written as below

```haskell
instance Functor ((->) r) where
	fmap = (.)
```

### `Applicative`

In `Applicatives`  we apply an  already wrapped  function over a  wrapped value.
They are defined in the  package `Control.Applicative`. An `Applicative` has the
following general type signature.

```haskell
class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```
Here `pure` which has the form of `a -> f a` is actually wrapping a
normal value with a default minimal context. This is called `lifting`
as its actually lifting a simple value to a wrapped value as defined.

For a function to act as an `Appllicative` it first needs to be a `Functor`.

>*Instances of `Applicative`*

```haskell
-- Maybe as an Applicative

instance Applicative Maybe where
	pure           = Just
	Nothing <*> _  = Nothing
	_ <*> Nothing  = Nothing
	(Just f) <*> x = Just (f x)
```

> *Some important layouts*
> an infix version of the `fmap` is `<$>` and is defined as below

```haskell
(<$>) :: (a -> b) -> f a -> f b
f <$> x = fmap f x
```
> based on the above...

```haskell
fmap f x = pure f <*> x = f <$> x = (<*>) . pure

-- the last one due to eta reduction
```

> `List` as an `Applicative` instance

```haskell
instance Applicative [] where
	pure x    = [x]
	fs <*> xs = [f x | f <- fs, x <- xs]
```

> `IO` as an `Applicative` instance

```haskell
instance Applicative IO where
	pure    = return
	a <*> b = do
		f <- a
		x <- b
		return (f x)
```

> `(->)` as an `Applicative` instance

```haskell
instance Applicative ((->) r) where
	pure x  = (\_ -> x)
	f <*> g = \x -> f x (g x)
```

> `ZipList` as an `Applicative` instance

`List` was already made an instance of `Applicative`. But if while combining two or more lists using a binary or a higher functon, if we want to apply the function to each element like first element of list1 with first element of list2 and so on we need to define in an alternative way. `ZipList` is there for the same purpose.

```haskell
instance Applicative ZipList where
	pure x                    = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

newtype ZipList a = ZipList { getZipList :: [a] }
```
#### *`Applicative` laws*

```haskell

pure id <*> v = v 								-- identity
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)    -- composition
pure f <*> pure x = pure (f x) 					-- homomorphism
u <*> pure y = pure ($ y) <*> u 				-- interchange
```

#### `Group and Semigroup`

In the  abstract mathematical sense,  we can  define a `Group`  `G` as a  set of
un-ordered unique elements associated with a binary operation such that


- for every element x, y in the Group G, `(x . y)` is also an element of G
- for every element x, y, z in the Group G, `x . (y . z) = (x . y) . z`
- there is some element e (referred to as identity element) in Group G such that for any element x in the Group G, `x . e = e . x = x`.
-  for any x in G, there exists some element y which satisfies the relation `x . y = y . x = e`. That is y is an inverse of x.

In haskell such a group can be defined in terms of type class as below

```haskell
class Group g where
    -- identity element
    identity :: g
    -- binary operation
    bin :: g -> g -> g
    -- inverse for an element
    inverse :: g -> g
```

Now whenever we want to create an instance of such a group, we need to verify that the following laws hold good.
>*the below is not a valid haskell code, but just a way of expressing the above laws*

```haskell
-- binary associative operation
forall x y z => x `bin` (y `bin` z) = (x `bin` y) `bin` z
-- an identity
forall x => identity `bin` x == x `bin` identity == x
-- inverse of every element
forall x => inverse x `bin` x == x `bin` inverse x == identity
```

For the actual real world use in haskell we prefer a much simplicied abstract concept than a group which is a Monoid.

In abstract matchematics a `monoid` is regarded as an algebraic datastructure with a single binary associative operation and an identity element. Monoids are an extension of the Semigroup with identity.

If `S` is a Set and `.` is a binary operation such that `S X S -> S`, then `S` together with `.` qualifies as a Monoid if it satisfies the below laws.

- Associativity
- Identity element

A `semigroup` together with an `identity` element is called a `monoid`
A `monoid` in which each each element has an inverse is called a `group`.


### `Monoids`

`Monoids` are types with an associative binary operation which have an identity. `Monoid` class is defined in the standard `haskell` as below

>*Note: if a `Monoid` does not satisfy the identity, then it is called a `Semigroup`. It exists in the `Data.Semigroup` package*

```haskell
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [a] -> a
	mconcat = foldr mappend mempty
```

`Monoids` are defined in the package `Data.Monoid`. The infix version of the function `mappend` is `(<>)` and is defined as below

```haskell
infix r6 <>
(<>) :: (Monoid m) => m -> m -> m
(<>) = mappend

-- laws governing the monoids
a <> (b <> c) = (a <> b) <> c -- associativity
mempty <> a = a 			  -- left identity
a <> mempty = a 			  -- right identity
```

>*Instances of a `Monoid` should satisfy the below laws*

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

> *instances of `monoid`*

```haskell
-- list as monoid
instance Monoid [a] where
	mempty  = []
	mappend = (++)

-- Maybe as monoid
instance (Monoid a) => Monoid (Maybe a)	where
	mempty                  = Nothing
	Nothing `mappend` m     = m
	m `mappend` Nothing     = m
	Just x `mappend` Just y = Just ( x `mappend` y)
```

#### `First` and the `Last` wrappers

They wrap  Maybe values to provide a new `monoid` instance.

* The `First` wrapper returns the left-most or first non Nothing value.

* The `Last` wrapper returns the right-most or last non Nothing value.

Here is how they are defined under `Data.Monoid`

```haskell
newtype First a = First { getFirst :: Maybe a }
				  deriving (Eq, Ord, Show, Read)

instance Monoid (First a) where
  mempty                         = First Nothing
  z@(First (Just _)) `mappend` _ = z
  First Nothing `mappend` z      = z

newtype Last a = Last { getLast :: Maybe a }
				 deriving (Eq, Ord, Show, Read)

instance Monoid (Last a) where
  mempty                        = Last Nothing
  _ `mappend` z@(Last (Just _)) = z
  z `mappend` Last Nothing      = z
```

* `Ordering` data type as a `Monoid`

```haskell
data Ordering = LT | GT | EQ

instance Monoid Ordering where
	mempty         = EQ
	LT `mappend` _ = LT
	EQ `mappend` y = y
	GT `mappend` _ = GT
```

### `Monads`

Here is the class definition of `Monad`

`Monad` is  a concept from a  branch of mathematics known  as category
theory. Monads are mathematical structures which can abstractly handle
uncertainty  (during   compile  times).  From   Haskell's  perspective
however, it  is best to  think of a monad  as an abstract  datatype of
actions.  Haskell's do  expressions  provide a  convenient syntax  for
writing monadic expressions.

- *Class definition of `Monad`*

```haskell
class Applicative m => Monad (m :: * -> *) where
   return :: a -> m a
   (>>=) :: m a -> (a -> m b) -> m b
   (>>) :: m a -> m b -> m b
   fail :: String -> m a
```

> All the instances of `Monads` should be `Applicative`s and `Functor`s prior to that.
> All the instances of `Monads` should satisfy the below three laws

```haskell
return x >>= f  = f x 						-- left identity
x >>= return    = x 						-- right identity
(m >>= f) >>= g = m >>= (\x -> f x >>= g) 	-- associativity
```

>*additionally instances of `Monad` and `Functor` should satisfy the below law*

```haskell
fmap f xs  ==  xs >>= return . f
```

> *here is how `List`, `Maybe`, `Either` and `(->)`are defined as instances of `Monad`*

```haskell
instance Monad [] where
   return x 	  = [x]
   xs >>= f 	  = concat (map f xs)
   [] >>= _       = []
   (x : xs) >>= f = f x ++ (xs >>= f)
   fail _ 		  = []

instance Monad Maybe where
   return 			= Just
   Just x >>= f 	= f x
   Nothing >>= _ 	= Nothing
   fail 			= Nothing

instance (Error e) => Monad (Either e) where
   return x       = Right x
   Right x >>= f  = f x
   Left err >>= f = Left err
   fail msg       = Left (strMsg msg)

instance Monad ((->) r) where
   return x = \_ -> x
   g >>= f  = \x -> f (g x) x
```

```haskell
-- simple example

λ> [0..9] >>= \x -> show (x + 1)
    "12345678910"
```

#### A note about Non-deterministic computations

>We can model non-determinism in `haskell` by returning a list of alternatives.
>A non-deterministic computation from `As` giving `Bs` becomes a function `A -> [B]`
>As an example we consider the list pairs of dice values summing up to a number

>*solution using list comprehensions*

```haskell
diceSum :: (Integral a) => a -> [(a, a)]
diceSum x = [(y1, y2) | y1 <- [1 .. 6], y2 <- [1 .. 6], y1 + y2 == x]
```

>but lists being `monads` we can have a monadic equivalent as below

```haskell
diceSum :: (Integral a) => a -> [(a, a)]
diceSum x = [1 .. 6] >>= \y1 ->
			[1 .. 6] >>= \y2 ->
			if y1 + y2 == x then return (y1, y2) else []
```

*One function which is useful in conditional checking is `guard` and its defined as below*

```haskell
guard :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero

-- and specifically for list its version will be

guard :: Bool -> [()]
guard True  = return ()
guard False = []

-- example
λ> ["abc","","","def","","ghi"] >>= \x -> guard (not (null x)) >> return x
    ["abc", "def", "ghi"]
```

- the below relation is valid as well from the `Monad` definition

```haskell
(>>) :: (Monad m) => m () -> m () -> m ()
x >> y = x >>= \_ -> y
```

### `MonadPlus`

`Maybe`, `Either` and `List` monads are quite similar in the sense that all of them represent the number of results a computation can have. They can all be used to represent computations which may fail.

Each of these monads can handle both SUCCESS and FAILURE

- for list `[xs]` FAILURE = empty list or `[]` and SUCCESS = `[xs]`, a Non-Empty list
- for `Maybe a` FAILURE = `Nothing` and SUCCESS = `Just a`
- for `Either a b` FAILURE = `Left a` and SUCCESS = `Right b`

Given two computations in one of these monads, it might be interesting to amalgamate the following:

* Find all valid solutions; for instance using lists, given 2 lists of valid solutions one can find all the valid solutions by simply concatenating the lists together.

* Find the failure case as discussed earlier.

We can combine these two features into a typeclass called `MonadPlus`

> *The `MonadPlus` type class is for `monads` which can also act as
> `monoids`*
>*They are the `Monads` which support Choice and Failure*

```haskell
class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a
```

* `mzero` is the monadic value standing for zero results.
* `mplus` is a binary function which combines two computations.

> *here is how `List` and `Maybe` are defined as instances of `MonadPlus`*

```haskell
instance MonadPlus [] where
   mzero = []
   mplus = (++)


instance MonadPlus Maybe where
   mzero                   = Nothing
   Nothing `mplus` Nothing = Nothing
   Just x `mplus` Nothing  = Just x
   Nothing `mplus` Just x  = Just x
   Just x `mplus` Just y   = Just x


import Control.Monad.Error
-- Either allows the failing computations to include an error message


instance (Error e) => MonadPlus (Either e) where
	mzero             = Left nomsg
	Left _ `mplus` m  = m
	Right x `mplus` _ = Right x
```

#### `MonadPlus` Laws

Instances of `MonadPlus` are required to follow certain laws similar to `Monads` as follows:


```haskell
mzero `mplus` m         = m 						  -- Monoid
m `mplus` mzero         = m
m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o

mzero >>= f  =  mzero 								  -- Left Zero
m >> mzero   =  mzero

(m `mplus` n) >>= k = (m >>= k) `mplus` (n >>= k)   -- Left Distribution
return m `mplus` n  = return a 						-- Left Catch
```

#### Additional `MonadPlus` functions

>Apart from the above defined `mzero` and `mplus` functions, there are a few other functions like the below:

>*`msum`*

One common task while working with the instances of `MonadPlus` is to take a list of the `monad`, for instance `[Maybe a]` or `[[a]]`, and `fold` then down the list with `mplus`. `msum` fulfills this role as defined below:

```haskell
msum :: MonadPlus m => [m a] -> m a
msum = foldr mplus mzero
```

A nice way of thinking about this is that it generalises the list-specific concat operation. Indeed, for lists, the two are equivalent. For `Maybe` it finds the first `Just x` in the list, or returns `Nothing` if there aren't any.

>*`guard`*

>The `guard` function is useful for conditional evaluation as defined earlier under the `Monad` section.

```haskell
guard :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero
```



#### Combinations

> `Functor`, `Applicative` and `Monad` are three of the most import and related classes of
> `Haskell`. If the `pure` and `return` are ignored for a moment, the characteristics methods of
>  the three can be summarized as below.

```haskell
(<$>) :: Functor t     =>   (a -> b) -> (t a -> t b)
(<*>) :: Applicative t => t (a -> b) -> (t a -> t b)
(=<<) :: Monad t       => (a -> t b) -> (t a -> t b)
```
*`haskell` adds some syntax sugar for conversion between `do` and `>>=`*

the below are equivalent

```haskell
do
   a <- f
   b <- g
   c <- h
   return (a, b, c)
```

which is same as...

```haskell
f >>= \a ->
   g >>= \b ->
      h >>= \c ->
         return (a, b, c)
```

#### `Kleisli Arrows`

There are some additional combinators for monads like `((>=>), (<=<))` which compose two different monadic actions in sequence.  The operation `(<=<)` is the monadic equivalent of the regular function composition operator `(.)` and `(>=>)` is same as `flip (<=<)`.

>definition of `>=>`  and `<=<`

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

>specifically using `((->) r)`

```haskell
f >=> g = \x -> f x >>= g
		= \x -> (\r -> g (f x r) r)

- or

(f >=> g) x r = g (f x r) r
```

*correspondence between `monadic` and `non monadic` compositions*

```haskell
(.)   ::            (b ->   c) -> (a ->   b) -> (a -> c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
```

*The 3 `monad` laws can be expressed in terms of the `kleisli arrows` as follows*

```haskell
return >=> f    = f 						-- Left Identity
f >=> return    = f 						-- Right Identity
(f >=> g) >=> h = f >=> (h >=> g) 			-- Associativity
```

>*miscellaneous identities*

```haskell
f >>= x = join (fmap f x)

fmap f x = x >>= (return . f)
join f   = f >>= id
```

#### Every `monad` is also a `functor`

We can derive the below relation to assert the statement

`fmap f x = x >>= return . f`

from the `((->) r)`

```haskell
fmap f x r 	= 	(x >>= (const . f)) r
			=	(const . f) (x r) r
			=	const (f (x r)) r
			=	f (x r)
			=	(f . x) r
```

#### `do` expression conversion

>*Translating `do` notation*

Each line `x <- e; ...` translates to `e >>= \x -> ...`
Each line `e; ...` trnaslates to `e >> ...`

- An example of the above translation

```haskell
do { x1 <- e1;
     x2 <- e2;
     e3;
     x4 <- e4;
     e5;
     e6  }

-- is equivalent to

     e1 >>= \x1 ->
     e2 >>= \x2 ->
     e3 >>
     e4 >>= \x4 ->
     e5 >>
     e6
```

>*an example with pythagorean triplets*

```haskell
pythagoreanTriplets :: (Integral a) => a -> [a]
pythagoreanTriplets n = do
	x <- [1 .. n]
	y <- [x .. n]
	z <- [y .. n]
	guard (x^2 + y^2 == z^2)
	return (x, y, z)
```

- re-writing the above function using chaining

```haskell
λ> import Control.Monad
λ> let pythagoreanTriplets n = [1 .. n] >>=
                               \x -> [x .. n] >>=
                               \y -> [y .. n] >>=
                               \z -> guard (x^2 + y^2 == z^2) >> return (x, y, z)
…
λ> :t pythagoreanTriplets
pythagoreanTriplets :: (Num t, Eq t, Enum t) => t -> [(t, t, t)]
λ> pythagoreanTriplets 5
    [(3, 4, 5)]
λ> pythagoreanTriplets 10
    [(3, 4, 5), (6, 8, 10)]
```
