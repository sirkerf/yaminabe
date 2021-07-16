import Data.Char
import Control.Monad.State
import System.IO

data Value = INT Integer | REAL Double deriving (Show, Eq)

data Token = Number Value
           | Ident String
           | Add | Sub | Mul | Div
           | Assign
           | Lpar | Rpar 
           | Semic
           | Comma
           | Eof
           | Others Char
    deriving (Show, Eq)

data Func = F1  (Value -> Calc Env Value)
          | F2  (Value -> Value -> Calc Env Value)

data Expr = Num Value
          | Var String
          | Op1 (Value -> Value) Expr
          | Op2 (Value -> Value -> Value) Expr Expr
          | Agn Expr Expr
          | App Func [Expr]

data Parse a = Fail | Err String | Some a deriving Show

instance Monad Parse where
    return x      = Some x
    Fail   >>= _  = Fail
    Err s  >>= _  = Err s
    Some a >>= k  = k a
    fail s        = Fail

instance MonadPlus Parse where
    mzero = Fail
    Fail   `mplus` ys = ys
    xs     `mplus` _  = xs

type Lexer  = (Token, String)
type Env    = [(String, Value)]
type Calc s a = StateT s Parse a

item :: Calc [a] a
item = do x:xs <- get
          put xs
          return x

lookahead :: Calc [a] a
lookahead = do x:_ <- get
               return x

failure :: Calc s a
failure = mzero

sat :: (a -> Bool) -> Calc [a] a
sat p = do x <- item
           if p then return x else failure

(+++) :: Calc s a -> Calc s a -> Calc s a
(+++) = mplus

calcError :: String -> Calc s a
calcError msg = StateT $ \_ -> Err msg

toREAL :: Value -> Double
toREAL (INT x)  = fromIntegral x
toREAL (REAL x) = x

callf1 :: (Double -> Double) -> Value -> Calc Env Value
callf1 f v = return $ REAL (f (toREAL v))

callf2 :: (Double -> Double -> Double) -> Value -> Value ->Calc Env Value
callf2 f v1 v2 = return $ REAL (f (toREAL v1) (toREAL v2))

callfri1 :: (Double -> Integer) -> Value -> Calc Env Value
callfri1 f v = return $ INT (f (toREAL v))

callfii1 :: (Integer -> Integer) -> Value -> Calc Env Value
callfii1 f (INT n) = return $ INT (f n)
callfii1 _ _       = calcError "Args is not Integer"

callfii2 :: (Integer -> Integer -> Integer) -> Value -> Value -> Calc Env Value
callfii2 f (INT n) (INT m) = return $ INT (f n m)
callfii2 _ _       _       = calcError "Args is not Integer"

--WIP