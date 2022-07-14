module Lib
    ( someFunc
    ) where

sum' :: Num a => a -> a -> a
sum' x y = x + y

someFunc :: IO ()
someFunc = print $ sum' 3 3
