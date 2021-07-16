import Data.List

main = do
    let objs = transpose ["パトカー", "タクシー"]
    putStrLn $ foldr (++) [] objs