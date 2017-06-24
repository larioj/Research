#!/usr/bin/env runhaskell

import Data.List (intercalate)

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
    
commutes' (*) =
  forAll bool $ \a ->
  forAll bool $ \b ->
    a * b == b * a

commutes (*) =
  all id
    (bool >>= \a  ->
     bool >>= \b  ->
       return (a * b == b * a))

associates' (*) =
  forAll bool $ \a ->
  forAll bool $ \b ->
  forAll bool $ \c ->
    (a * b) * c == a * (b * c)

associates (*) =
  all id
    (bool >>= \a ->
     bool >>= \b ->
     bool >>= \c ->
       return ((a * b) * c == a * (b * c)))

identity (*) =
  any id
    ((flip map) bool (\i ->
      all id
        (bool >>= \x ->
           return ((i*x == x) && (x*i == x)))))

inverses (*) =
  any id
    ((flip map) bool (\i ->
      all id
        ((flip map) bool (\x ->
          any id
            ((flip map) bool (\y ->
              ((i*x == x) && (x*i == x) && (x*y == i) && (y*x == i))))))))

tally (index, op) =
  intercalate " " 
    ((pad 5 ("a" ++ (show index))) :
      (tabs (commutes op) "commutes") :
      (tabs (associates op) "associates") :
      (tabs (identity op) "identity") :
      (tabs (inverses op) "inverses") :
      [])
   where
     pad n str = take n (str ++ (replicate n ' '))
     tabs cond desc = if cond then (pad 10 desc) else (pad 10 "")

main = putStrLn $ intercalate "\n" $ map tally (zip [0..] ops)
