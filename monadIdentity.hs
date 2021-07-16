data Identity a = Identity a deriving Show
instance Monad Identity where
    return x = Identity x
    (Identity x) >>= f = f x