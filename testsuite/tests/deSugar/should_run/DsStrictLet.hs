{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

data C a = C a

main :: IO ()

main = let x :: a -> a
           !x = \a -> a
       in return ()


-- main = let (a, b) = trace "evaluated b" ('a', '1')
--        in return a

-- main =
--   let !(C a) = C undefined
--   in return ()

-- main = let C a b c = C '1' '2' '3'
--        in putChar a >> return ()
