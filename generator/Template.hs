module Template where

import Control.Monad
import System.IO

import Glyph

argify :: String -> ([String],String)
argify (x:xs)               = go (x:xs) [] where
  go ('-':'-':'>':xs) accum = (accum:[],xs)
  go (' ':xs) accum         = let (a,b) = go xs [] in (accum:a, b)
  go (x:xs)   accum         = go xs (accum ++ [x])
argify xs                   = ([],xs)

transformXMLFile :: FilePath -> IO String
transformXMLFile f = do
  s <- readFile f
  transformXML s

transformXML :: String -> IO String
transformXML ('<':'!':'-':'-':'M':'E':'K':'F':'N':'T':' ':xs)
  = let (args,rest) = argify xs
        process = case args of
                    ("glyph":a) -> genGlyphA a
                    (c:a)       -> error $ "Unknown font command " ++ show c
    in do suffix <- transformXML rest
          prefix <- process
          return $ prefix ++ suffix
transformXML (x:xs) = do rest <- transformXML xs
                         return $ x:rest
transformXML _      = return ""
