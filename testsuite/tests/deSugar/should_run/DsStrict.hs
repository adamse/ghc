{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Main where
import Debug.Trace



f0 a = "fun"
f0' ~a = "fun'"

f1 ~n = case n of
          a -> "case"
f1' ~n = case n of
           ~a -> "f1'"


f2 = \a -> "lamda"
f2' = \ ~a -> "lambda'"

f3 ~n = let a = n
        in "let"
f3' ~n = let ~a = n
         in "let'"


newtype Age = MkAge Int

f4, f4' :: Age -> String
f4 (MkAge a) = "newtype"
f4' ~ (MkAge a) = "newtype'"

main :: IO ()
main = mapM_ (\(n, f) -> putStrLn (f (v n))) fs
  where fs = [("fun", f0)
             ,("fun'", f0')
             ,("case", f1)
             ,("case'", f1')
             ,("lambda", f2)
             ,("lambda'", f2')
             ,("let", f3)
             ,("let'", f3')
             ,("newtype", (\ ~i -> f4 (MkAge i)))
             ,("newtype'", (\ ~i -> f4' (MkAge i)))]
        v n = trace ("evaluated in " ++ n) 1
