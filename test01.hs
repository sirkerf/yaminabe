n = div a (length xs) where
    a = 10
    xs = [1,2,3,4,5]

main :: IO ()
main = do
    print n