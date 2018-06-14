module Glyph where

import Control.Monad
import Data.Char
import Data.List
import System.FilePath.Glob
import System.IO

import Util

--                    <path/>,w  ,h
getGlyph :: String -> (String,Int,Int)
getGlyph svg =
  let (rp,w,h) = go svg 8 8
      p        = dropWhites (map normalizer rp)
      normalizer x = if x `elem` "\t\n\r" then ' ' else x
      dropWhites (' ':' ':xs) = dropWhites $ ' ':xs
      dropWhites (x:xs)       = x : dropWhites xs
      dropWhites xs           = xs
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

unTranslate :: String -> String
unTranslate ('<':'p':'a':'t':'h':xs) =
  let (dx,dy) = findTs xs
  --in  "<!-- " ++ show dx ++ "," ++ show dy ++ "--><path" ++ puukotaP (dx,dy) xs
  in  "<path" ++ puukotaP (dx,dy) xs
  where findTs ('/':'>':xs) = (0,0)
        findTs ('t':'r':'a':'n':'s':'l':'a':'t':'e':'(':r0) =
          let (x,r1) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r0
              (y,r2) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r1
          in  (x,y)
        findTs (x:xs) = findTs xs
        findTs xs     = (0,0)
        puukotaP dc ('/':'>':xs) = "/>" ++ unTranslate xs
        puukotaP dc (' ':'t':'r':'a':'n':'s':'f':'o':'r':'m':'=':'"':xs) = del xs where
          del ('"':xs) = puukotaP dc xs
          del (x:xs)   = del xs
          del xs       = puukotaP dc xs
        puukotaP (dx,dy) ('d':'=':'"':xs) = "d=\"" ++ pp xs where
          pp ('"':xs) = '"':puukotaP (dx,dy) xs
          pp ('M':r0) =
            let (x,r1) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r0
                (y,r2) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r1
                nx     = x + dx
                ny     = y + dy
            in  'M': show nx ++ " " ++ show ny ++ pp r2
          pp (x:xs) | x == 'H' || x == 'V' = let (oi,rest) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) xs
                                                 ni        = oi + (case x of 'H' -> dx
                                                                             'V' -> dy)
                                             in  x : show ni ++ pp rest
          pp (x:xs) = x : pp xs
          pp xs = puukotaP (dx,dy) xs
        puukotaP ds (x:xs) = x : puukotaP ds xs
        puukotaP ds xs     = unTranslate xs
unTranslate (x:xs) = x:unTranslate xs
unTranslate xs     = xs

-- for a single glyph
extractD :: String -> String
extractD ('<':'p':'a':'t':'h':xs) = go xs where
  go ('d':'=':'"':xs) = "d=\"" ++ go2 xs where
    go2 ('"':xs) = "\""
    go2 (x:xs)   = x:go2 xs
    go2 xs       = ""
  go (x:xs) = go xs
  go xs = ""
extractD (x:xs) = x:extractD xs
extractD xs = xs

-- for a single glyph
convertY :: Int -> String -> String
convertY height ('d':'=':'"':xs) = "d=\"" ++ go xs where
  read1 r0 = let (y,r1) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r0
             in  (y,r1)
  read2 r0 = let (x,r1) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r0
                 (y,r2) = readint $ dropWhile (\i -> i /= '-' && (i < '0' || i > '9')) r1
             in  (x,y,r2)
  go ('"':xs) = '"' : xs
  go (x:xs) =
    x : case x of
          'M' -> let (x,y,r) = read2 xs
                 in  show x ++ " " ++ show (height - y) ++ go r
          'm' -> let (x,y,r) = read2 xs
                 in  show x ++ " " ++ show (-y) ++ go r
          'V' -> let (y,r) = read1 xs
                 in  show (height - y) ++ go r
          'v' -> let (y,r) = read1 xs
                 in  show (-y) ++ go r
          _ -> go xs
convertY height (x:xs) = x : convertY height xs
convertY height xs = ""

genSingleGlyph :: (String,String,Int,Int) -> String
genSingleGlyph (unicode,path,width,height) = "<glyph unicode=\"" ++ xmlEscape unicode ++ "\" horiz-adv-x=\"" ++ show (width - 1) ++ "\" " ++ convertY (height - 2) (extractD (unTranslate path)) ++ "/>"

genGlyphA :: [String] -> IO String
genGlyphA [c,f,u] = genGlyph c f u
genGlyphA args    = error $ "Wrong number of arguments to genGlyphA!"

genGlyph :: String -> String -> String -> IO String
genGlyph category file unicode = do
  glyphs <- getGlyphsF category file unicode
  return $ intercalate "\n" (map genSingleGlyph glyphs)
