--data Parser s a = Parser {runParser :: s -> (a, s)}

item :: Parser s s
item = do x:xs <- get
          put xs
          return x
item2 :: Parser s [s]
item2 = do a <- item
           b <- item
           return [a, b]

failure :: Parser s a
failure = mzero

sat :: (s -> Bool) -> Parser s s
sat p = do x <- item
           if p x then return x else failure

(+++) :: Parser s a -> Parser s a -> Parser s a
(+++) = mplus

many :: Parser s a -> Parser s [a]
many p = many1 p +++ return []

many1 :: Parser s a -> Parser s [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

number :: Parser Char Integer
number = do
    xs <- many1 $ sat isDigit
    return (read xs)
