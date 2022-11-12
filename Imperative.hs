module Imperative
    ( if_    , if'
    , when   , (?>)
    , unless , (!>)
    , foreach, foreach', (|>), (#|>)
    , while  , while'
    , switch , match , (<=>)
    , after  , (.:)
    , after' , (.:.)
    , just   , just' )
where

-- Church If, Void.
if_ :: (Monad m) => Bool -> m a -> m a -> m ()
if_ True  a _ = a >> return ()
if_ False _ b = b >> return ()

-- Church If, A.
if' :: (Monad m) => Bool -> m a -> m a -> m a
if' True  a _ = a
if' False _ b = b

-- When clause, Void.
when :: (Monad m) => m a -> Bool -> m ()
when m True  = m >> return ()
when m False = return ()

-- When clause, infix, Void.
(?>) :: (Monad m) => m a -> Bool -> m ()
(?>) = when

-- Unless clause, Void.
unless ::(Monad m) => m a -> Bool -> m ()
unless m b = m `when` not b

-- Unless clause, infix, Void.
(!>) :: (Monad m) => m a -> Bool -> m ()
(!>) = unless

-- Foreach, Void.
foreach :: (Monad m) => [a] -> (a -> m b) -> m ()
foreach []     _ = return ()
foreach (x:xs) f = f x >> foreach xs f

-- Foreach, infix, Void.
(|>) :: (Monad m) => [a] -> (a -> m b) -> m ()
(|>) = foreach

-- Foreach, tail accumulator, A.
foreach' :: (Monad m) => a -> [b] -> (a -> b -> m a) -> m a
foreach' accumulator []     _ = return accumulator
foreach' accumulator (x:xs) f = f accumulator x >>= \result -> foreach' result xs f

-- Foreach, infix, tail accumulator, A.
(#|>) :: (Monad m) => [b] -> a -> (a -> b -> m a) -> m a
(#|>) xs accumulator = foreach' accumulator xs

-- While, Void.
while :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m ()
while initial predicate each do' =
    if_ (predicate initial) (
        do' initial >> while (each initial) predicate each do'
    ) $
        return ()

-- While, tail accumulator, A.
while' :: (Monad m) => a -> b -> (b -> Bool) -> (b -> b) -> (a -> b -> m a) -> m a
while' accumulator initial predicate each do' =
    if' (predicate initial) (
        do' accumulator initial >>= \result -> while' result (each initial) predicate each do'
    ) $
        do' accumulator initial

-- Switch, Void.
switch :: (Monad m) => a -> [((a -> Bool), m b)] -> m ()
switch element [] = return ()
switch element (x:xs) =
    if_ (predicate element) (
        m >> return ()
    ) $
        switch element xs

    where
        predicate = fst x
        m         = snd x

-- Match, A (Switch, A).
match :: (Monad m, Eq a) => a -> b -> [((a -> Bool), m b)] -> m b
match element default' []     = return default'
match element default' (x:xs) =
    if' (predicate element) (
        m
    ) $
        match element default' xs

    where
        predicate = fst x
        m         = snd x

-- Use in conjunction to `match'.
(<=>) :: (Monad m, Eq a) => a -> b -> ((a -> Bool), m b)
(<=>) a b = ((==) a, return b)

-- Composition, (unary . binary).
after :: (a -> b) -> (a -> a -> a) -> a -> a -> b
after f g a b = f $ g a b
(.:) = after -- infix

-- Composition, (unary . `trinary').
after' :: (a -> b) -> (a -> a -> a -> a) -> a -> a -> a -> b
after' f g a b c = f $ g a b c
(.:.) = after' -- infix

-- Cast binary to monadic binary.
just :: (Monad m) => (a -> a -> a) -> (a -> a -> m a)
just = after return

-- Cast `trinary' to monadic `trinary'.
just' :: (Monad m) => (a -> a -> a -> a) -> (a -> a -> a -> m a)
just' = after' return
