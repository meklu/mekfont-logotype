module Glyph where

import Control.Monad
import Data.Char
import Data.List
import System.FilePath.Glob
import System.IO

--                    <path/>,w  ,h
getGlyph :: String -> (String,Int,Int)
getGlyph svg =
  let (rp,w,h) = go svg 8 8
      p        = dropWhites (map normalizer rp)
      normalizer x = if x `elem` "\t\n\r" then ' ' else x
      dropWhites (' ':' ':xs) = dropWhites $ ' ':xs
      dropWhites (x:xs)       = x : dropWhites xs
      dropWhites xs           = xs
      readint :: String -> (Int,String)
      readint xs                                   = readint' xs ""
      readint' (x:xs) accum | x >= '0' && x <= '9' = readint' xs $ accum ++ [x]
                            | otherwise            = (read accum :: Int,x:xs)
      go :: String -> Int -> Int -> (String,Int,Int)
      -- extract glyph dimensions
      go ('<':'s':'v':'g':' ':xs) w h =
        let dimensions ('/':'>':xs)                         = go xs w h
            dimensions ('w':'i':'d':'t':'h':'=':'"':xs)     = let (w,rest) = readint xs
                                                              in  go rest w h
            dimensions ('h':'e':'i':'g':'h':'t':'=':'"':xs) = let (h,rest) = readint xs
                                                              in  go rest w h
            dimensions (x:xs) = dimensions xs
            dimensions xs     = go xs w h
        in dimensions xs
      go ('<':'p':'a':'t':'h':xs) w h =
        let pathProc ('/':'>':xs) = let (s,_,_) = go xs w h
                                    in  ('/':'>':s,w,h)
            -- strip the style attribute which is only used for fill color anyway
            pathProc ('s':'t':'y':'l':'e':'=':'"':xs) = stripStyle xs where
              stripStyle ('"':xs) = pathProc xs
              stripStyle (x:xs)   = stripStyle xs
              stripStyle xs       = go xs w h
            -- decrease x-translation by 1
            pathProc ('t':'r':'a':'n':'s':'l':'a':'t':'e':'(':xs) = translate xs where
              translate xs =
                let (x,rest) = readint $ dropWhile (\x -> x < '0' || x > '9') xs
                    (s,w,h)  = pathProc rest
                in  ("translate(" ++ show (x - 1) ++ s,w,h)
            pathProc (x:xs) = let (s,w,h) = pathProc xs
                                in  (x:s,w,h)
            pathProc _      = go xs w h
            -- final assembly
            (s,_,_) = pathProc xs
        in  ('<':'p':'a':'t':'h':s,w,h)
      go (x:xs) w h = go xs w h
      go xs     w h = ("",w,h)

  in (p,w,h)

getGlyphF :: String -> String -> IO (String,String,Int,Int)
getGlyphF fpath unicode = do
  svg <- readFile fpath
  let (p,w,h) = getGlyph svg
  let lastSlash = head . reverse $ elemIndices '/' fpath
  let basename = takeWhile (/='.') $ drop (lastSlash + 1) fpath
  let funicode = case unicode of
                   ('$':x:xs) -> case (x:xs) of
                                   "pass"  -> basename
                                   "upper" -> map toUpper basename
                                   "lower" -> map toLower basename
                                   s       -> error $ "Unknown special action" ++ show s ++ "!"
                   s          -> s
  return (funicode,p,w,h)

getGlyphsF :: String -> String -> String -> IO [(String,String,Int,Int)]
getGlyphsF category file unicode = do
  let pttrn = "../glyphs/" ++ category ++ "/" ++ file ++ ".svg"
  files <- glob pttrn
  mapM (\x -> getGlyphF x unicode) (sort files)

xmlEscape :: String -> String
xmlEscape ('<':xs) = '&':'l':'t':';': xmlEscape xs
xmlEscape ('>':xs) = '&':'g':'t':';': xmlEscape xs
xmlEscape ('&':xs) = '&':'a':'m':'p':';': xmlEscape xs
xmlEscape ('"':xs) = '&':'q':'u':'o':'t':';': xmlEscape xs
xmlEscape ('\'':xs) = '&':'a':'p':'o':'s':';': xmlEscape xs
xmlEscape (x:xs)   = x : xmlEscape xs
xmlEscape xs       = xs

genSingleGlyph :: (String,String,Int,Int) -> String
genSingleGlyph (unicode,path,width,height) = "<glyph unicode=\"" ++ xmlEscape unicode ++ "\" horiz-adv-x=\"" ++ show (width - 2) ++ "\">" ++ path ++ "</glyph>"

genGlyphA :: [String] -> IO String
genGlyphA [c,f,u] = genGlyph c f u
genGlyphA args    = error $ "Wrong number of arguments to genGlyphA!"

genGlyph :: String -> String -> String -> IO String
genGlyph category file unicode = do
  glyphs <- getGlyphsF category file unicode
  return $ intercalate "\n" (map genSingleGlyph glyphs)
