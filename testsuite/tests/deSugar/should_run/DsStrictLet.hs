{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

data C a = C a

poly :: a
poly = poly

main = let !(a, b) = (trace "was here" (poly, \a -> a)) :: (b, a -> a)
       in return ()


-- main = let (a, b) = trace "evaluated b" ('a', '1')
--        in return a

-- main =
--   let !(C a) = C undefined
--   in return ()

-- main = let C a b c = C '1' '2' '3'
--        in putChar a >> return ()
