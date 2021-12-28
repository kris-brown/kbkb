module ToHTML ( makeSite, addHtml) where
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
import Control.Monad (when, forM_, unless)
import Database.PostgreSQL.Simple (Connection, execute, query_, query, Binary(..))
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory,
                         removeDirectoryRecursive, createDirectory, renameFile,
                         listDirectory, copyFile, removeFile)

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
 | ls /= lb = error $ show ls <> " != " <> show lb
 | otherwise = do body <- sequence ioProcessedReg
                  bklinks <- backLinks cn s
                  nav <- mkNav cn s
                  overallHead <- pack <$> readFile "src/html/overallhead.html"
                  let res = concat [[if' outer (overallHead <> titl <> nav) ""], body,
                                    [bklinks], if' outer [footnote, "</body></html>"] [""]]
                  return $ T.concat res
  where ss' = filter isSections ss
        (ls, lb) = (length ss', length bs)
        reg = getRegions s t --regions (zip ss' bs) $ T.length t
        ioProcessedReg = processRegion cn t <$> reg
        titl = "\n<h1 class=\"title\">"<> title m <>"</h1>"
fixHTML _ _ _ _ _ _ = undefined

getRegions :: Section -> Text -> [Either (Int, Int) (Section, Bracket)]
getRegions (Sections _ ss _ ) t = regions (zip ss' bs) $ T.length t
  where (Bracket _ _ bs) = getBracket t 0
        ss' = filter isSections ss
getRegions _ _ = undefined

repMany :: [(Text,Text)] -> Text -> Text
repMany rs x = foldl (\h (n,r) -> replace n r h) x rs


processRegion :: Connection -> Text -> Either (Int, Int) (Section, Bracket)
          -> IO Text
processRegion db t (Left (i, j)) = processPlainText db $ takeDrop (i-1) (j+1) t
processRegion db t (Right (s@(Sections (MData ttl tg uuid) ss _),
                            Bracket i j bs)) = do
  u <- url db s
  let subText = takeDrop i j t
  subContent <- fixHTML db False s (getBracket subText 0) subText ""
  return $ T.concat ["\n<details id=\"", uuid,
    "\">\n<summary style=\"background:",
    tagToColor tg,";\" id=\"",uuid,"\"> \n\t<strong>", mkHref u ttl,
    "</strong>\n</summary>\n<div id=\"", uuid,"\">",
    subContent, "\n</div></details>\n"]
processRegion _ _ _ = undefined
mkNav :: Connection -> Section -> IO Text
mkNav c s = do
  tp <- query_ c "SELECT urlpth FROM section WHERE uuid='root'"
  up <- query c q1 [u] :: IO [[Text]]
  nxt <- query c (q2<>"+1") [u] :: IO [[Text]]
  prv <- query c (q2<>"-1") [u] :: IO [[Text]]
  let t = mkLnk "Home" tp
  let u = mkLnk "Up" up
  let n = mkLnk "Next" nxt
  let p = mkLnk "Previous" prv
  return $ T.concat ["<nav><p> ", t, u, p, n, "</p></nav>"]
  where u = uid $ mdata s
        q1 = "SELECT s2.urlpth FROM section AS s1 JOIN section AS s2 ON \
              \(s2.id=s1.parent) WHERE s1.uuid=?"
        q2 = "SELECT s2.urlpth FROM section AS s1 JOIN section AS s2 ON \
                \(s2.parent=s1.parent) WHERE s1.uuid=? AND s2.ord = s1.ord"
        mkLnk _ [] = ""
        mkLnk txt [[x]] = " "<>mkHref x txt<>" "
        mkLnk _ _ = undefined

mkHref :: Text -> Text -> Text
mkHref a b = T.concat ["<a href=\"",rootURL, a,".html\">", b,"</a>"]

backLinks :: Connection -> Section -> IO Text
backLinks c s = do
  lData <- query c q [uid $ mdata s]
  let lnks = f <$> lData
  return $ if' (null lnks) ""
               ("<h3>Linked by</h3><ul>" <> intercalate "\n" lnks <> "</ul>")
    where
      q = "SELECT src.urlpth, comm  FROM link \
      \JOIN section as tgt ON (link.tgt=tgt.id) \
      \JOIN section as src ON (link.src=src.id) \
      \WHERE tgt.uuid = ?"
      f (urlpth, desc) = T.concat [
        "<li><a href=\"", rootURL, urlpth, ".html\">", desc, "</a></li>"]

-- Fix internal links
processPlainText :: Connection -> Text -> IO Text
processPlainText c t =  do
  ls <- query_ c "SELECT repl, display, urlpth FROM link JOIN section ON (link.tgt=section.id)"
  let ls' = f <$> ls
  return $ repMany ls' t
    where f (a,b,c) = ("<span class=\"math inline\">\\(\\ref{" <> a <> "}\\)</span>", T.concat ["<a href=\"",rootURL, c,".html\">", b,"</a>"])

-- Connection to web database, path to the web folder, and section to convert
initialHTML :: Connection -> FilePath -> Section -> IO (Text, Text, [(Text,BS.ByteString)])
initialHTML c fp s = do
  u <- unpack <$> url c s
  tmpdir <- (<> u) <$> getTemporaryDirectory
  luaText <- pack <$> readFile "src/misc/tikz-to-png.lua"
  let [l1, l2] = splitOn "site" luaText
  let tmplua = tmpdir <> "my.lua"
  writeFile tmplua $ unpack $ l1 <> pack fp <> l2
  createDirectoryIfMissing True tmpdir
  let texpth = tmpdir <> "/test.tex"
  writeFile texpth $ unpack $ toLatex s Nothing
  system $ "pandoc -f latex -t html --toc --quiet --mathjax --citeproc \
  \--bibliography=bib/my.bib --csl=bib/ieee.csl --from latex+raw_tex \
  \--lua-filter="<> tmplua <> " -s -o " <> tmpdir <> "/test.html "<>texpth

  -- Move generated images
  pngs <- filter isPng <$> listDirectory "."
  pngdata <- sequence $ BS.readFile <$> pngs
  let imgs = zip (pack <$> pngs) pngdata

  -- Process the raw html result
  res <- readFile (tmpdir ++ "/test.html")
  let [_,res'] = splitOn "</header>" $ pack res
  let [res'',_] = splitOn "</body>" res'
  let (rbody', rfoot) = (case splitOn (fstDelim res) res'' of
                [a,b] -> (a, footdelim <> b)
                [a] -> (a, "")
                _ -> undefined)
  let rbody = strip rbody'
  let assert = if' (take nStart rbody == startDelim) 1 (error $ "badStart " <> unpack rbody)
  let assert' = if' (takeEnd nEnd rbody == endDelim) 1 (error $ "badEnd " <> unpack rbody)
  assert `seq` pure ()
  assert' `seq` pure ()
  let resbody = takeDrop (nStart+1) (T.length res''-nEnd-2) rbody
  return (resbody, rfoot, imgs)
  where footdelim = "<section class=\"footnotes"
        bibdelim = "<div id=\"refs\" class=\"references"
        fstDelim x = if' (isInfixOf bibdelim $ pack x) bibdelim footdelim
        isPng p = P.drop (length p - 4) p == ".png"
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
 | curr < i = Left (curr, i-1) : regions' sbs tL i
 | curr == i = Right sb : regions' sbs' tL (j+1)
   where sb@(_, Bracket i j _) = head sbs
         sbs' = tail sbs
regions' _ _ _ = undefined


-- Code related to parsing a nested structure with start and end delimiters
---------------------------------------------------------------------------
findIndices :: Text -> Text -> [Int]
findIndices needle haystack = fst <$> filter ((needle `isPrefixOf`) . snd) (
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
pairBracket _ = undefined

pairBracketRec :: [(Int, Bool)] -> Int -> Int -> Int
pairBracketRec _ 0 index = index
pairBracketRec ((i, True):parens) numOpen index =
  pairBracketRec parens (numOpen + 1) (index + 1)
pairBracketRec ((i, False):parens) numOpen index =
  pairBracketRec parens (numOpen - 1) (index + 1)
pairBracketRec _ _ _ = undefined

applyPairBracket :: [(Int, Bool)] -> [(Int, Int)]
applyPairBracket [] = []
applyPairBracket p@((i, True):parens) = (i, nEnd + fst (p !! pairBracket p)):applyPairBracket parens
applyPairBracket ((i, False):parens) = applyPairBracket parens

pairBrackets :: Text -> [(Int, Int)]
pairBrackets t = applyPairBracket startends
  where [starts, ends] = (`findIndices` t) <$> [startDelim, endDelim]
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

-- Connection to web DB and path to site folder
addHtml :: Connection -> FilePath -> Section -> IO ()
addHtml c _ Content {} = pure ()
addHtml c fp s@(Sections (MData ttl _ u) ss _) = do
  (iHTML, iFootnote, imgs) <- initialHTML c fp s
  let imgs' = [(a,Binary b) | (a,b)<-imgs]
  forM_ imgs' (execute c q2) -- add images to DB
  forM_ (fst <$> imgs) (removeFile . unpack) -- remove image files created
  let starts = findIndices startDelim iHTML
  let b = getBracket iHTML 0
  t <- fixHTML c True s b iHTML iFootnote
  rs <- reps c
  let t' = repMany rs t
  execute c q (t', u)
  sequence_ $ addHtml c fp <$> ss
    where q = "UPDATE section SET html = ? WHERE uuid = ?;"
          q2 = "INSERT INTO img (iname, ival) VALUES (?,?)"
reps :: Connection -> IO [(Text, Text)]
reps c = do us <- query_ c "SELECT urlpth FROM section;" :: IO [[Text]]
            return $ (urlfix <$> us) ++ fixed
  where urlfix [u] = ("href=\""<>u <> "\"",
                      T.concat ["href=\"", rootURL, u, ".html\""])
        urlfix _ = undefined
        fixed = [(startDelim, ""), (endDelim, ""),
                ("img src=\"", "img src=\""<>rootURL<>"img/"),
                ("src=\"img/", "src=\"" <> rootURL <> "img/"), -- from \includegraphics
                ("role=\"doc-bibliography\">",
                    "role=\"doc-bibliography\"><strong>Bibliography</strong><br>")]

-- TODO: the fact that site/ is the target site folder is hardcoded into
-- tikz-to-png.lua
makeSite :: Connection -> FilePath -> IO ()
makeSite c sitepth = do
  -- initialize site/img directory with img/ directory and img table
  createDirectoryIfMissing True $ sitepth <> "/img/"
  imgs <- listDirectory "img"
  sequence_ $ (\x -> copyFile ("img/"<>x) (sitepth <> "/img/"<>x)) <$> imgs
  dbImgs <- query_ c "SELECT iname, ival FROM img"
  sequence_ $ f <$> dbImgs
  -- copy toplevel files
  sequence_ $ (\x-> copyFile ("src/html/"<>x) (sitepth<>"/"<> x)) <$> [
    "demo.css", "jquery.minipreview.js", "jquery.minipreview.css"]

  -- Add the pages
  urlhtmls <- query_ c "SELECT urlpth, html FROM section"
  sequence_ $ addUrlHtml sitepth <$> urlhtmls
    where f (iname,ival) = BS.writeFile (sitepth <> "/img/" <> iname) ival

--
addUrlHtml :: FilePath -> (Text,Text) -> IO ()
addUrlHtml fp (u, h) = do
  createDirectoryIfMissing True $ fp <> dir
  writeFile (fp <> unpack u <> ".html") (unpack h)
    where dir = unpack $ intercalate "/" $ init (splitOn "/" u)
