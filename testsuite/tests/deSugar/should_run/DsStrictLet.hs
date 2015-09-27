{-# LANGUAGE Strict #-}
module Main where

import Debug.Trace

main = let False = trace "no binders" False

           a :: a -> a
           a = trace "polymorphic" id

           f :: Eq a => a -> a -> Bool
           f = trace "overloaded" (==)

           xs :: [Int]
           xs = (trace "recursive" (:) 1 xs)
       in return ()
