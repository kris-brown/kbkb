module FixHtml (fixHtml) where

import Text.XML (Document(..), parseText_, readFile, writeFile,
                 def, Element(Element, elementName, elementAttributes, elementNodes),
                 Node(NodeElement, NodeContent),
                 Name(nameLocalName) )
import Prelude hiding (readFile, writeFile, dropWhile, lookup, null, concat)
import qualified Prelude as P
import Debug.Trace (trace)
import qualified Data.Text as T
import Data.Text (concat, pack, unpack, Text, takeEnd, split, isPrefixOf, toLower,
                  replace, intercalate, strip, null, splitOn, dropWhile)
import Data.Map (Map, (!), toList, lookup, keys, insert, member,
                 findWithDefault)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Char (isDigit, isSpace, isAscii)
import qualified Data.Text.Internal.Lazy as L
import qualified Data.Text.Lazy as LZ
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (copyFile, removeFile)
import Text.Regex (Regex, mkRegex, subRegex)
import Text.RawString.QQ ( r )

-- Utilities
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- Constants
root :: Text
root = "https://web.stanford.edu/~ksb/phil/"

jsbody :: Text
jsbody = pack $ unsafePerformIO $ P.readFile "src/html/jsbody.html"
jshead :: Text
jshead = replace "demo" (root <> "demo") $ pack $ unsafePerformIO $ P.readFile "src/html/jshead.html"

ne :: Node -> Maybe Element
ne (NodeElement x) = Just x
ne x = Nothing

-- Does processing that requires knowledge of HTML AST.
-- Modifies TOC with fixTOC. Adds previews and collapsible sections.
procc :: Document -> Document
procc x = Document (documentPrologue x) newElem (documentEpilogue x)
  where dr@(Element n1 n2 _) = documentRoot x
        [dhead, dbody@(NodeElement(Element b1 b2 _))] = noSpaceNodes dr
        Just (titl:toc:rst) = noSpaceNodes <$> ne dbody
        (rst_sum, rst_body) = findNextH rst
        fixed = titl : fixTOC toc : rst_sum ++ fixBody rst_body
        newElem = Element n1 n2 [dhead, NodeElement (Element b1 b2 fixed)]

-- Fix TOC by replacing the "URL | name" entries with hrefs
-- SHOULD WE ADD .html to the end and lower case here?
fixTOC :: Node -> Node
fixTOC (NodeElement (Element e1 e2 e3)) = NodeElement $ if' (member "href" e2 && isJust prts)
  (Element e1 (insert "href" s1 e2) [NodeContent s2])
  (Element e1 e2 $ fixTOC <$> e3)
    where prts = case e3 of [NodeContent s] -> case split (=='|') s of
                                                  [a,b] -> Just [a,b]
                                                  _ -> Nothing
                            _ -> Nothing
          [s1, s2] = fromJust prts
fixTOC x = x


-- We need to make sections collapsible as well as fix+preview links.
-- We can assume the first section here is a h1
-- TO DO. handle references/footnotes?
fixBodyDetail :: [Node] -> [Node]
fixBodyDetail [] = []
fixBodyDetail [x] = error $ "UNEXPECTED " <> show x
fixBodyDetail (x:xs) = (NodeElement . empdet <$> ps) ++ xtra
  where (ps, xtra) = hpartition (x:xs)
        empdet [] = error "empdet on empty"
        empdet (z:zs) = case det z of
          Element e1 e2 [e3,e4,e5,NodeElement (Element e6 e7 [])] ->
            let (zs1, zs2) = findNextH zs
            in Element e1 e2 [e3,e4,e5,NodeElement (
                Element e6 e7 $ zs1 ++ fixBodyDetail zs2)]
          qqq -> error $ show qqq

findNextH :: [Node] -> ([Node],[Node])
findNextH = break isHeader

isHeader :: Node -> Bool
isHeader y = Just True == (mtch . unpack . nameLocalName . elementName <$> ne y)
  where mtch str = (head str == 'h') && all isDigit (tail str)

fixBody :: [Node] -> [Node]
fixBody = fixBodyDetail -- . concatMap addPreviews

-- Create a section of backlinks (if there are any)
makeBacklinks :: [(Text, Text)] -> Text
makeBacklinks [] = ""
makeBacklinks bklinks = "<h3>Linked by</h3><ul>" <> lnks <> "</ul>"
  where lnks = intercalate "\n" $ f <$> bklinks
        f (x, y) = let desc = if' (null y) "" ("#"<>y) in concat [
          "<li><a href=\"", toLower $ nospace (fixPth' x), desc, "\">",
          if' (null y) (fixPth' $ last (splitOn "/" x)) y , "</a></li>"]

nospace :: Text -> Text
nospace = T.filter (not . isSpace)

-- Given an id for a summary and its color, get a pattern to replace
colorReps :: (Text, Text) -> (Text,Text)
colorReps (sid, scolor) = res
  where idstr = "id=\"" <> sid <> "\"" -- <> ".html"
        res = ("summary " <> idstr, "summary " <> scolor <> idstr)
-- Backlinks used to generate additional replacements
-- Colordict used to add color to summary elements.
reps::Text -> Map Text [(Text, Text)] -> Map Text Text -> [(Text,Text)]
reps name bklinks colorDict =  (colorReps <$> toList colorDict) ++
  ((\x->(x, nospace x)) <$> keys bklinks) ++ [
  ("</body>", makeBacklinks $ findWithDefault [] (toLower name) bklinks),
  ("img src=\"", "img src=\""<>root<>"img/"),
  ("src=\"img/", "src=\"" <> root <> "img/"), -- images from \includegraphics
  ("href=\"doc", "href=\"" <> root <> "doc"),
  ("src=\"doc", "src=\"" <> root <> "doc"),
  ("role=\"doc-bibliography\">", "role=\"doc-bibliography\"><strong>Bibliography</strong><br>"),
  -- Insert code into head and body
  ("</head>",jshead), ("</body>",jsbody),
  -- Change width
  ("max-width: 36em", "max-width: 80em"),
  -- Some things get mangled by Text.XML, fix them:
  ("li &gt; ol, li &gt; ul", "li > ol, li > ul"),
  ("&#39;Lucida Console&#39;","'Lucida Console'"),
  -- Change font
  ("line-height: 1.5;", "line-height: 1.5;\n      font-family: \"Caslon Pro\", \"Georgia\", serif !important;"),
  ("margin: 0 auto;", "margin: 0 auto;\n      font-family: \"Caslon Pro\", \"Georgia\", serif !important;"),
  ("margin-top: 1.4em;", "margin-top: 1.4em;\n      font-family: \"Caslon Pro\", \"Georgia\", serif !important;"),
  -- Add </script>
  ("<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js\" type=\"text/javascript\"/>",
   "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js\" type=\"text/javascript\"/></script>")]

-- IS THIS WHERE WE ADD .html and lowercase?
fixInternalLinks:: Text -> Text
fixInternalLinks x = intercalate delim $ head chunks : (f <$> tail chunks)
  where chunks = splitOn delim x
        delim = "href=\"doc"
        f chunk = let (hd:tl) = splitOn "\"" chunk in
                  intercalate "\"" $ g hd : tl
        g = (\x -> if' (takeEnd 4 x == ".pdf") x (x <> ".html")) . toLower . nospace . fixPth'

-- Text version of fixPth
fixPth' :: Text -> Text
fixPth' = pack . fixPth . unpack

-- Remove the numeric prefixes used to order sections
fixPth :: String -> String
fixPth s = unpack $ intercalate "/" splt
  where p x = isDigit x || isSpace x
        splt = dropWhile p <$> (splitOn "/" $ pack s)

-- Take an html file and replace it with a fixed version (backup of orig and
-- intermediate states stored)
fixHtml :: Text -> Map Text [(Text, Text)] -> Map Text Text ->
            FilePath -> Maybe FilePath -> IO ()
fixHtml name bkLnks colorDict fp' tgt =
  do pure ()--copyFile fp $ tmp 0
     d <- readFile def fp
     writeFile def (tmp 1) (procc d)
     txt <- P.readFile (tmp 1)
     let regmod = pack $ mapNonAscii appRegs txt
     --P.writeFile (tmp 2) $ unpack regmod -- post regex version
     let intLnk = fixInternalLinks regmod
     --P.writeFile (tmp 3) $ unpack intLnk -- post regex version
     let repmod = foldl (\h (n,r) -> replace n r h) intLnk $ rs
     length txt `seq` P.writeFile (fromMaybe fp tgt) $ unpack repmod
     removeFile $ tmp 1
  where tmp i = take (length fp - 5) fp <> "._" <> show i <> ".html"
        fp = fixPth fp'
        rs = reps name bkLnks colorDict -- all simple text substitutions

-- Make an EMPTY summary element from a header element
det :: Node ->Element
det i@(NodeElement (Element _ a [NodeContent b])) = documentRoot $ parseText_ def $
  LZ.fromStrict $ "<details id=\"" <> hid <> "\" " <>
    --" open=\"open\"" <>  -- UNCOMMENT IF YOU WANT SECTIONS TO START OPEN
    ">\n" <> "<summary id=\"" <> hid <> "\"> <strong>" <>
    "<a href=\"" <> nospace (fixPth' hurl) <>"\">" <> hname <> "</a>\n" <> "</strong>\n" <>
    "</summary>" <> "\n<div id=\"" <> hid <> "\"></div></details>\n"
  where hid = a ! "id"
        [hurl, hname] = split (=='|') b
det _ = undefined

-- Group content under a single header. Assumes the first node is a header.
-- Trailing content (footnotes/bibliography) stored in a separate list
hpartition :: [Node] -> ([[Node]], [Node])
hpartition (n:tl) = (reverse foldres1, reverse foldres2)
  where f (h:t, xtra) nextNode = if' (not (P.null xtra) || isBib nextNode)
                                       (h:t, nextNode:xtra)
                                       (if' (lev nextNode == toplev)
                                            ([nextNode]:h:t, xtra)
                                            ((h++[nextNode]):t, xtra))
        f _ _ = undefined
        (foldres1, foldres2) = foldl f ([[n]], []) tl
        toplev = lev n
        lev x = nameLocalName . elementName <$> ne x
        isBib x = maybe False (\e -> lookup "id" e == Just "refs"
                                  || lookup "role" e == Just "doc-endnotes")
                              (elementAttributes <$> ne x)
hpartition _ = undefined

-- Filter all pure whitespace node contents
noSpaceNodes :: Element -> [Node]
noSpaceNodes = filter (maybe True (not . null . strip) . nc) . elementNodes
  where nc (NodeContent x) = Just x
        nc x = Nothing


-- Makes a mouseover preview box
box :: Text -> Node
box lnk = NodeElement $ documentRoot $ parseText_ def $ LZ.concat [
  "<div class=\"box\"> ","<iframe src=\"", LZ.fromStrict lnk,
  "\" width = \"500px\" height = \"500px\"> </iframe> </div>"]



appRegs :: String -> String
appRegs txt = foldl (\t (s,r) -> subRegex r t s) txt regs1


strSectionsRec :: Bool -> (a->Bool) -> [a] -> [[a]]
strSectionsRec True _ [] = []
strSectionsRec False _ [] = [[]]
strSectionsRec curr f s = h : strSectionsRec (not curr) f t
  where (h, t) = (if' curr span break) f s
strSections = strSectionsRec True isAscii
pairList :: [a] -> [(a,a)]
pairList [] = []
pairList (a:b:c) = (a,b):(pairList c)
pairList x = error $ show $ length x
unPairList :: [([a],[a])] -> [a]
unPairList [] = []
unPairList ((a,b):tl) = a ++ b ++ (unPairList tl)

-- Apply transformation function to ASCII portions of str
mapNonAscii :: (String -> String) -> String -> String
mapNonAscii f s = unPairList $ zip (f <$> ascii) nonascii
  where (ascii, nonascii) = unzip $ pairList $ strSections s


tx = P.readFile "/Users/ksb/code/kbkb/site/doc/phil/People/Sellars/Quotes0.html"
regs1 :: [(String, Regex)]
regs1 = fmap mkRegex <$> [
   -- (1) Remove the post-pipe comment in internal links
  ([r|href="doc\1" id="\2"|],
   [r|href=\"doc([^\"]+)\|([^\"]+)\"|])]
--    -- (2) headers -> details
--   ([r|<details id="\1" open="open">
-- <summary id="\1"> <strong><a href="\2">\3</a></strong> </summary>
-- <div id="\1">|],
--    [r|<h[0-9]+ id=\"(.+)\">(.+)\|(.+)<.+>|])]

---------------------
-- Other functions --
---------------------
-- Add </div> based on headers
-- addDivs :: String -> String
-- addDivs = unlines . reverse .  snd . foldl addDivsRec (0,[]) . lines
-- getH :: String -> Maybe Int
-- getH = fmap (fmap (read . head)) . matchRegex $ mkRegex [r|<h([0-9]+)+ id=".+">.+<.+>|]
-- addDivsRec :: (Int, [String]) -> String -> (Int, [String])
-- addDivsRec (n, res) curr
--  | n < 0 = (n, curr : res )
--  | any (\t-> t `isInfixOf` pack curr)  ["doc-endnote","doc-bibliography","</body>"] = (-1, curr:P.replicate n enddiv ++ res)
--  | otherwise = case getH curr of
--   Nothing -> (n, curr:res)
--   Just x -> (x, curr:P.replicate (if n<x then 0 else n-x+1) enddiv ++ res)
-- Need to remove the unicode arrow that is used for footnotes
-- fixHtml :: FilePath -> IO ()
-- fixHtml fp = do copyFile fp (fp <> ".bkup")
--                 txt <- concatMap (\case '\8617' -> ['^'];'\65038'->[];c-> [c]) <$> readFile fp
--                 let regmod = foldl (\t (s,r) -> subRegex r t s) (addDivs txt) regs1
--                 let regmod2 = foldl (\t (s,r) -> subRegex r t s) regmod regs2
--                 length txt `seq` writeFile fp regmod2

  -- The up/prev/next buttons need previews and to have their links corrected
-- addPreviews :: Node -> [Node]
-- addPreviews n@(NodeElement (Element e1 e2 e3)) =
--   if' (nameLocalName e1 == "a" && member "href" e2 && e3 /= [NodeContent "PDF"])
--     [n, box $ e2 ! "href"]
--     [if' (isHeader n) n (
--       NodeElement (Element e1 e2 $ concatMap addPreviews e3))]
-- addPreviews x@(NodeContent _) = [x]
-- addPreviews _ = undefined


--   fromJust $
--   do e@(Element e1 e2 _) <- ne n
--      e'@(Element e1' e2' _) <- ne $ head $ noSpaceNodes e
--      let fixed = fixTOCelem False <$> noSpaceNodes e'
--      return $ NodeElement $ Element e1 e2 [
--               NodeElement $ Element e1' e2' fixed]

-- -- If flag is True, then we use the first half before the '|' to change the
-- -- hyperlink, otherwise we keep it the same (this is the option we use in TOC)
-- fixTOCelem :: Bool -> Node -> Node
-- fixTOCelem b (NodeElement (Element f1 f2 [NodeElement (Element f1' f2' [
--   NodeContent s])])) = NodeElement (Element f1 f2 [NodeElement (
--     Element f1' (if' b (insert "href" s1 f2') f2') [NodeContent s2])])
--   where [s1, s2] = split (=='|') s
-- fixTOCelem _ x = error (show x)