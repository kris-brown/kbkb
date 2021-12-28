module ToHTML ( ) where
import Prelude hiding (take, drop, intercalate)
import qualified Prelude as P
import Data.Text (Text, pack, unpack, breakOn, isPrefixOf, tails, findIndex,
          takeEnd, take, drop, splitOn, replace, strip, isInfixOf, intercalate)
import qualified Data.Text as T
import Text.RawString.QQ ( r )
import System.Process (system)
import Data.Tuple.Extra (fst3)
import Data.List (sort)
import Debug.Trace (trace)
import Control.Monad (when)
import Database.PostgreSQL.Simple (Connection, execute, query_)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory,  removeDirectoryRecursive, createDirectory)

import ToLatex
import DB
rootURL = "https://web.stanford.edu/~ksb/phil/"
-- Keep track of positions of starts and ends of sections
data Bracket = Bracket Int Int [Bracket] deriving (Show, Eq, Ord)
startDelim = "<p>SECTIONSTART</p>"
endDelim = "<p>SECTIONEND</p>"
nStart = T.length startDelim
nEnd = T.length endDelim

-- Modify HTML
fixHTML :: Connection -> Bool -> Section -> Bracket -> Text -> Text -> IO Text
fixHTML cn outer s@(Sections m ss c) (Bracket _ _ bs) t footnote
 | ls /= lb = error $ (show ls) <> " != " <> (show lb)
 | otherwise = do body <- sequence ioProcessedReg
                  overallHead <- pack <$> readFile "src/html/overallhead.html"
                  let res = (if' outer overallHead ""):body ++ (if' outer [footnote, "</body></html>"] [""])
                  return $ T.concat res
  where ss' = filter isSections ss
        (ls, lb) = (length ss', length bs)
        reg = getRegions s t --regions (zip ss' bs) $ T.length t
        ioProcessedReg = (processRegion cn t) <$> reg


getRegions :: Section -> Text -> [Either (Int, Int) (Section, Bracket)]
getRegions (Sections _ ss _ ) t = regions (zip ss' bs) $ T.length t
  where (Bracket _ _ bs) = getBracket t 0
        ss' = filter isSections ss

repMany :: [(Text,Text)] -> Text -> Text
repMany rs x = foldl (\h (n,r) -> replace n r h) x rs

applyReps :: Text -> Text
applyReps = repMany reps

processRegion :: Connection -> Text -> Either (Int, Int) (Section, Bracket)
          -> IO Text
processRegion db t (Left (i, j)) = processPlainText db $ takeDrop (i-1) (j+1) t
processRegion db t (Right (s@(Sections (MData ttl tg uuid) ss _),
           (Bracket i j bs))) = do
  u <- url db s
  let subText = takeDrop i j t
  subContent <- fixHTML db False s (getBracket subText 0) subText ""
  return $ T.concat ["\n<details id=\"", uuid,
    "\">\n<summary style=\"background:",
    tagToColor tg,";\" id=\"",uuid,"\"> \n\t<strong><a href=\"",u,"\">",
    ttl,"</a></strong>\n</summary>\n<div id=\"", uuid,"\">",
    subContent, "\n</div></details>\n"]

-- Fix internal links
processPlainText :: Connection -> Text -> IO Text
processPlainText c t = do
  ls <- query_ c "SELECT repl, display, urlpth FROM link JOIN section ON (link.tgt=section.id)"
  let ls' = f <$> ls
  print ls'
  return $ repMany ls' t
    where f (a,b,c) = ("<span class=\"math inline\">\\(\\ref{" <> a <> "}\\)</span>", T.concat ["<a href=\"",rootURL, c,".html\">", b,"</a>"])


initialHTML :: Connection -> Section -> IO (Text, Text)
initialHTML c s = do
  u <- unpack <$> url c s
  tmpdir <- (<> u) <$> getTemporaryDirectory
  createDirectoryIfMissing True tmpdir
  let texpth = tmpdir <> "/test.tex"
  writeFile texpth $ unpack $ toLatex s Nothing
  system $ "pandoc -f latex -t html --toc --quiet --mathjax --citeproc \
  \--bibliography=bib/my.bib --csl=bib/ieee.csl --from latex+raw_tex \
  \--lua-filter=src/misc/tikz-to-png.lua -s -o " <> tmpdir <> "/test.html "<>texpth
  res <- readFile (tmpdir ++ "/test.html")
  let [_,res'] = splitOn "</header>" $ pack res
  let [res'',_] = splitOn "</body>" res'
  let (rbody', rfoot) = (case splitOn (fstDelim res) res'' of
                [a,b] -> (a, footdelim <> b)
                [a] -> (a, ""))
  let rbody = strip rbody'
  let assert = if' (take nStart rbody == startDelim) 1 (error $ "badStart " <> unpack rbody)
  let assert' = if' (takeEnd nEnd rbody == endDelim) 1 (error $ "badEnd " <> unpack rbody)
  assert `seq` (pure ())
  assert' `seq` (pure ())
  let resbody = takeDrop (nStart+1) ((T.length res'')-nEnd-2) rbody
  return $ (resbody, rfoot)
  where footdelim = "<section class=\"footnotes"
        bibdelim = "<div id=\"refs\" class=\"references"
        fstDelim x = if' (isInfixOf bibdelim $ pack x) bibdelim footdelim

-- Convert this type into a more printable type for debugging
viewRegions :: [Either (Int, Int) (Section, Bracket)] -> [(Text, Int, Int)]
viewRegions = fmap f
  where f (Left (i,j)) = ("Left",i,j)
        f (Right (_,Bracket i j _)) = ("Right",i,j)

regions :: [(Section,Bracket)] -> Int -> [Either (Int, Int) (Section, Bracket)]
regions sbs totLen = regions' sbs totLen 0

regions' :: [(Section,Bracket)] -> Int -> Int
              -> [Either (Int, Int) (Section, Bracket)]
regions' [] tL curr = [Left (curr,tL)]
regions' sbs tL curr
 | tL == curr = []
 | curr < i = (Left (curr, i-1)):regions' sbs tL i
 | curr == i = (Right sb):regions' sbs' tL (j+1)
   where sb@(_, Bracket i j _) = head sbs
         sbs' = tail sbs



-- Code related to parsing a nested structure with start and end delimiters
---------------------------------------------------------------------------
findIndices :: Text -> Text -> [Int]
findIndices needle haystack = fst <$> (filter ((needle `isPrefixOf`) . snd) $
                                       zip [0..] (tails haystack))

-- Assuming that the start and end of the text are startDelim and endDelim
getBracket :: Text -> Int -> Bracket
getBracket t off =  Bracket off (off + T.length t)
          [getBracket t' off' | (t',off') <- toffs]
  where pairs = pairBrackets t
        tls  = topLevelPairedBrackets pairs
        toffs = [(takeDrop (i + nStart) (j- nEnd) t, off + i + nStart)
            | (i,j) <- tls]
-- Get slice of Text given start and end index
takeDrop :: Int -> Int -> Text -> Text
takeDrop i j = take (j-i) . drop i

-- have a sequence of parens (True = open, False = close)
-- assuming the first one is an open, find the index of the closing one
pairBracket :: [(Int, Bool)] -> Int
pairBracket ((i,True):parens) = pairBracketRec parens 1 0

pairBracketRec :: [(Int, Bool)] -> Int -> Int -> Int
pairBracketRec _ 0 index = index
pairBracketRec ((i, True):parens) numOpen index =
  pairBracketRec parens (numOpen + 1) (index + 1)
pairBracketRec ((i, False):parens) numOpen index =
  pairBracketRec parens (numOpen - 1) (index + 1)

applyPairBracket :: [(Int, Bool)] -> [(Int, Int)]
applyPairBracket [] = []
applyPairBracket p@((i, True):parens) = (i, nEnd + (fst $ p !! pairBracket p)):applyPairBracket parens
applyPairBracket ((i, False):parens) = applyPairBracket parens

pairBrackets :: Text -> [(Int, Int)]
pairBrackets t = applyPairBracket startends
  where [starts, ends] = (\x->findIndices x t) <$> [startDelim, endDelim]
        startends = sort $ [(i, True) | i <- starts]
            ++ [(i, False) | i <- ends]

-- Filter paired brackets to hide nested ones
topLevelPairedBrackets :: [(Int, Int)] -> [(Int, Int)]
topLevelPairedBrackets xs = topLevelPairedBrackets' xs 0

topLevelPairedBrackets':: [(Int, Int)] -> Int -> [(Int, Int)]
topLevelPairedBrackets' [] _ = []
topLevelPairedBrackets' ((i,j):xs) currMax
 | i < currMax = topLevelPairedBrackets' xs currMax
 | otherwise = (i,j):topLevelPairedBrackets' xs j

tagToColor :: TagType -> Text
tagToColor Kris = "lightpink"
tagToColor Def = "lightgreen"
tagToColor Prop = "mediumpurple"
tagToColor Exercise = "lightsalmon"
tagToColor Example = "powderblue"
tagToColor Default = "lightgrey"

addHtml :: Connection -> Section -> IO ()
addHtml c Content {} = pure ()
addHtml c s@(Sections (MData ttl _ u) ss _) = do
  putStrLn $ ">>>title: " <> unpack ttl <> "\n>>>number of subSections: " <> show (length (filter isSections ss))
  (iHTML, iFootnote) <- initialHTML c s
  let starts = findIndices startDelim iHTML
  let b = getBracket iHTML 0
   --if' (null starts) (Bracket 0 (T.length iHTML) [])
  --            (getBracket iHTML (head starts))
  when (u=="root") $ writeFile "test.html" (unpack iHTML) -- debugging
  print $ ">>>b: " <> show b
  t <- fixHTML c True s b iHTML iFootnote
  let t' = applyReps t
  putStrLn $ ">>> t: " <> unpack t
  --putStrLn $ "t' " <> unpack t'
  execute c q (t', u)
  sequence_ $ addHtml c <$> ss
  where q = "UPDATE section SET html = ? WHERE uuid = ?;"

reps :: [(Text, Text)]
reps = [(startDelim, ""), (endDelim, ""),
        ("role=\"doc-bibliography\">",
         "role=\"doc-bibliography\"><strong>Bibliography</strong><br>")]

makeSite :: Connection -> FilePath -> IO ()
makeSite c sitepth = do
  urlhtmls <- query_ c "SELECT urlpth, html FROM section"
  sequence_ $ addUrlHtml sitepth <$> urlhtmls

addUrlHtml :: FilePath -> (Text,Text) -> IO ()
addUrlHtml fp (u, h) = do
  createDirectoryIfMissing True $ fp <> dir
  writeFile (fp <> unpack u <> ".html") (unpack h)
    where dir = unpack $ intercalate "/" $ init (splitOn "/" u)

test = do
  resetDB
  c <- normc
  c' <- webc
  s <- loadDir "bkup_doc"
  toDB c s
  popWeb c c'
  addURL c' "" s
  addTeX c' s
  addHtml c' s
  removeDirectoryRecursive "site2"
  createDirectory "site2"
  makeSite c' "site2/"
