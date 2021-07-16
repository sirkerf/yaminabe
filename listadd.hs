import Control.Monad.State

f1 :: State s (Int -> Int)
f1 = return (1 +)

f2 :: State s (Int -> Int)
f2 = state $ \s -> ((1 +), s)

f3 :: [Int -> Int]
f3 = return (1 +)

main = do
    print $ (evalState f1 ()) 1
    print $ (evalState f2 ()) 1
    print $ (f3 !! 0) 1