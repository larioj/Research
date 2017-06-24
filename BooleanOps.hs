#!/usr/bin/env runhaskell

import           Data.List (intercalate)

forAll :: [a] -> (a -> Bool) -> Bool
forAll = flip all

exists :: [a] -> (a -> Bool) -> Bool
exists = flip any

bool = [False, True]
ops =
  bool >>= \a ->
  bool >>= \b ->
  bool >>= \c ->
  bool >>= \d ->
    let an False False = a
        an False True  = b
        an True False  = c
        an True True   = d
    in return an

commutes (*) =
  forAll bool $ \a ->
  forAll bool $ \b ->
    a * b == b * a

associates (*) =
  forAll bool $ \a ->
  forAll bool $ \b ->
  forAll bool $ \c ->
    (a * b) * c == a * (b * c)

identity (*) =
  exists bool $ \id ->
  forAll bool $ \x ->
    (id*x == x)
      && (x*id == x)

inverses (*) =
  exists bool $ \id ->
  forAll bool $ \x ->
  exists bool $ \inv ->
    (id*x == x)
      && (x*id == x)
      && (x*inv == id)
      && (inv*x == id)

names =
  "zero"   :
    "and"  :
    ""     :
    "fst"  :
    ""     :
    "snd"  :
    "xor"  :
    "or"   :
    "nor"  :
    "iff"  :
    "nsnd" :
    ""     :
    "nfst" :
    ""     :
    "nand" :
    "one"  :
    []

tally (name, (index, op)) =
  intercalate " "
    ( (pad 7 name) :
      (pad 5 ("a" ++ (show index))) :
      (tabs (commutes op) "commutes") :
      (tabs (associates op) "associates") :
      (tabs (identity op) "identity") :
      (tabs (inverses op) "inverses") :
      [])
   where
     pad n str = take n (str ++ (replicate n ' '))
     tabs cond desc = if cond then (pad 10 desc) else (pad 10 "")

main = putStrLn $ intercalate "\n" $ map tally (zip names (zip [0..] ops))
