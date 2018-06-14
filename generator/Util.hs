module Util where

readint :: String -> (Int,String)
readint xs                                     = readint' xs "" where
  readint' ('-':xs) accum                      = readint' xs $ accum ++ ['-']
  readint' (x:xs) accum | x >= '0' && x <= '9' = readint' xs $ accum ++ [x]
  readint' xs     accum                        = readint'' xs accum
  readint'' xs    []                           = (0,xs)
  readint'' xs    accum                        = (read accum :: Int,xs)
