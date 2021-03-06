module Template where

import Control.Monad
import Data.List
import System.IO

import Glyph
import Util

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
transformXML xs = trs "" xs >>= \s -> return $ scaleDimensions s
  where
    trs ident ('\n':xs) = trs ""              xs >>= \s -> return $ '\n' : s
    trs ident (' ':xs)  = trs (ident ++ " ")  xs >>= \s -> return $ ' '  : s
    trs ident ('\t':xs) = trs (ident ++ "\t") xs >>= \s -> return $ '\t' : s
    trs ident ('<':'!':'-':'-':'M':'E':'K':'F':'N':'T':' ':xs)
      = let (args,rest) = argify xs
            indentA iol = do parts <- iol
                             return $ intercalate ('\n':ident) parts
            process = case args of
                        ("glyph":a) -> indentA $ genGlyphA a
                        (c:a)       -> error $ "Unknown font command " ++ show c
        in do suffix <- trs ident rest
              prefix <- process
              return $ prefix ++ suffix
    trs ident (x:xs) = do rest <- trs (ident ++ " ") xs
                          return $ x:rest
    trs ident _      = return ""

-- stupid stuff, pls ignore
scaleDimensions :: String -> String
scaleDimensions ('<':'f':'o':'n':'t':xs) = "<font" ++ multiply xs where
  passAttr ('"':xs) = '"':multiply xs
  passAttr ( x :xs) =  x :passAttr xs
  passAttr xs       = multiply xs
  multiply ('f':'o':'n':'t':'-':'w':'e':'i':'g':'h':'t':'=':'"':xs) = "font-weight=\"" ++ passAttr xs
  multiply ('u':'n':'i':'c':'o':'d':'e':'=':'"':xs) = "unicode=\"" ++ passAttr xs
  multiply (x:xs) | x >= '0' && x <= '9' = let (oi,rest) = readint (x:xs)
                                               nis       = show $ oi * 128
                                           in  nis ++ multiply rest
                  | otherwise            = x : multiply xs
  multiply xs                            = scaleDimensions xs
scaleDimensions (x:xs) = x : scaleDimensions xs
scaleDimensions xs     = xs

buildTpl :: FilePath -> FilePath -> IO ()
buildTpl infile outfile = do
  s <- transformXMLFile infile
  writeFile outfile s
