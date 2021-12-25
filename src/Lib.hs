
module Lib
  (sectionToHTML, getSections
  ) where
import Prelude hiding (concat, replicate, dropWhile)
import qualified Prelude as P
import Data.Text (Text, unpack, pack, splitOn, concat, replicate, intercalate,
                  replace, isInfixOf, dropWhile, toLower)
import qualified Data.Text as T
import System.Process (system)
import System.Directory (listDirectory, doesFileExist, copyFile,
                         createDirectoryIfMissing, renameFile, removeDirectory,
                         getTemporaryDirectory, withCurrentDirectory)
import Control.Monad (forM, forM_, when)
import Data.Either (partitionEithers)
import Data.Char(isSpace, isDigit, isAlphaNum)
import Data.Map (Map, fromList, toList, insertWithKey', elems, keys, unions,
                 singleton)
import qualified Data.Map as M
import Data.List (sortOn, elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)
import Text.RawString.QQ ( r )
import Debug.Trace (trace)
import FixHtml (fixHtml)
---------------------
-- Data structures --
---------------------
data TagType = Kris | Def | Prop | Exercise | Example
                deriving (Show, Eq, Ord, Read)

newtype MetaData = MetaData{tag::Maybe TagType} -- what else can we add
                    deriving (Show, Eq, Ord)

data Section = Sections {sname::Text, sintro::Text, sbody::[Section]}
             | Notes {nname::Text, nbody::Text}
             deriving (Show, Eq, Ord)

-- Parse the opening lines of a tex file that look like "% ORD 2" / "% TAG Def"
parseMetaData :: Text -> MetaData
parseMetaData t = MetaData  (res "TAG")
  where tlines = fmap unpack <$> takeWhile (\l->not (T.null l) && (T.head l == '%')) $ T.lines t
        reg x = mkRegex ("\\% " <> x <> " (.*)")
        res' x = listToMaybe $ mapMaybe (fmap head . matchRegex (reg x)) tlines
        res x =  read <$> res' x
        -- (res "ORD") (pack <$> res' "GIST")

isSect :: Section -> Bool
isSect Sections {} = True
isSect _ = False

-- The path of the section, e.g. phil/Phil Quotes/Sellars/Scientium Mensura
title :: Section -> Text
title (Sections t _ _) = t
title (Notes t  _) = t

-- The title of the section
title' :: Section -> Text
title' =  last . splitOn "/" . title''

title'':: Section -> Text
title'' = multiDropDigitSpace . title

dropDigitSpace :: Text -> Text
dropDigitSpace = dropWhile (\x->isDigit x || isSpace x)
multiDropDigitSpace :: Text -> Text
multiDropDigitSpace = intercalate "/" . fmap dropDigitSpace . splitOn "/"

md :: Section -> MetaData
md = parseMetaData . (\case
  Sections _ t _ -> t
  Notes _ t ->  t)

-------------------
-- Basic helpers --
-------------------
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

nospace :: Text -> Text
nospace = T.filter (not . isSpace)

---------------
-- Constants --
---------------
packages :: [Text]
packages = ["tikz", "tikz-cd", "hyperref", "amsmath", "amssymb", "amsthm"]

preamb :: Text
preamb = intercalate "\n" $ ["\\documentclass[12pt,a4paper]{report}"] ++ ((
  \p -> "\\usepackage{"<> p <> "}") <$> packages) ++ [
    "\\usetikzlibrary{arrows,positioning}",
    "\\tikzset{ >=stealth', punkt/.style={ rectangle, rounded corners, \
    \draw=black, very thick, text width=6.5em,minimum height=2em, text \
    \centered}, pil/.style={ ->, thick, shorten <=2pt, shorten >=2pt,}}",
    "\\begin{document}\n"]

hierarchy :: [Text]
hierarchy = ["part", "chapter", "section", "subsection", "subsubsection",
             "paragraph", "subparagraph"]

--------------------
-- Main functions --
--------------------
--Get all links in a body of text
linkMatch :: Text -> [(Text, Text)]
linkMatch = fmap (splt . linkend) . linkstarts
  where linkstarts = tail . splitOn "\\href{doc/"
        linkend = splitOn "|" . head . splitOn "}"
        splt [a] = ("doc/"<>toLower (multiDropDigitSpace a), "")
        splt [a,b] = ("doc/"<> toLower (multiDropDigitSpace a), b)
        splt _ = undefined

linkBody :: Text -> Text -> Map Text [(Text, Text)]
linkBody n t = M.singleton (toLower $ multiDropDigitSpace n) $ linkMatch t

getInternalLinks' :: Section -> Map Text [(Text, Text)]
getInternalLinks' (Notes n b) = linkBody  n b
getInternalLinks' (Sections n i b) = unions $
  linkBody n i : (getInternalLinks' <$> b)

-- Switch from links TO pages to links FROM pages (+ comment)
invertLinkList :: Map Text [(Text, Text)] -> Map Text [(Text, Text)]
invertLinkList m = --trace (unpack $ intercalate "\n" $ pack . show <$> toList m) $
                    nonempty $ fromList $ f <$> keys m
  where trips = concatMap (\(a, bcs) -> (\(b,c)-> (b, a, c)) <$> bcs) $ toList m
        f key = (key, (\(a,b,c)->(b,c)) <$> filter (\(x,_,_) -> key == x) trips)
        nonempty = M.filter (not . null)

getInternalLinks :: Section -> Map Text [(Text, Text)]
getInternalLinks = invertLinkList . getInternalLinks'

tagToColor :: MetaData -> Text
tagToColor (MetaData m) = " style=\"background:" <> (case m of
                              Just Kris     -> "lightpink"
                              Just Def      -> "lightgreen"
                              Just Prop     -> "mediumpurple"
                              Just Exercise -> "lightsalmon"
                              Just Example  -> "powderblue"
                              Nothing       -> "lightgrey"
                            ) <> ";\" "

getColors :: Section -> Map Text Text
getColors n@(Sections _ _ ss) = unions $ singleton (processColorName n)
                                          (tagToColor $ md n):(getColors<$>ss)
getColors n@(Notes _ _) = singleton (processColorName n) $ tagToColor $ md n

processColorName :: Section -> Text
processColorName s =  toLower $ T.filter p (title'' s <> f (title' s))
  where p x = isAlphaNum x || x == '-'
        f =  T.replace " " "-"
-- Parse a TOP LEVEL section from directory of tex files
getSections :: FilePath -> IO Section
getSections fp = do
  is_fi <- doesFileExist fp
  if' is_fi
    (do txt <- pack <$> readFile fp
        let ntitl = concat $ init $ splitOn "." $ pack fp -- remove extension
        return $ Notes ntitl txt)
    (do children' <- fmap ((fp <> "/") <>) <$> listDirectory fp
        children <- sequence $ getSections <$> children'
        let intros = filter isIntro children
        let intro = if' (null intros) "" (case head intros of
                                          (Notes _ x) -> x
                                          (Sections n _ _) -> trace (show n) "")
        let unordNonIntro = filter (not . isIntro ) children
        let nonintros = sortOn title unordNonIntro
        return $ Sections (pack fp) intro nonintros)
   where
         isIntro x = toLower (title' x) == toLower (dropDigitSpace ( last (
           splitOn "/" $ pack fp)))


latexBody :: Int -> Section -> Text
latexBody _ (Notes _ n) = n
latexBody n (Sections _ i ss) = concat $ tc : i : map (f n) ss
 where tc = if' (n == 0) "\n\\tableofcontents\n" ""
       f n s = concat ["\n\\",hierarchy !! n, "{",
                       nospace (title'' s) <> "|" <> title' s,
                       "}\n", latexBody (n+1) s]

refPat :: Text -> Text -> Text
refPat x y = " \\href{"<>x<>"}{"<>y<>"} "

-- Make a complete latex file for each
toLatex::Section -> Maybe Section -> Text
toLatex x parent = concat [preamb, upprevnext, pdflink, "\n\\title{", title' x,
                           "}\n", lb,cit, "\n\\end{document}"]
  where
    lb = latexBody 0 x
    hascite = "\\cite" `isInfixOf` lb
    cit = if' hascite "\n\\bibliography{my}\n\\bibliographystyle{amsalpha}" ""
    upprevnext = upPrevNext x parent
    pdflink = "\n" <> refPat (nospace $ title'' x <> ".pdf") "PDF"

-- Generate hyperlinks for the Up / Prev / Next buttons
upPrevNext :: Section -> Maybe Section -> Text
upPrevNext _ Nothing = ""
upPrevNext s (Just p@(Sections _ _ ss)) = intercalate "\n" [
    if' (i > 0) (refPat (pths!!(i-1)) "Previous") "",
    refPat (nospace (title'' p)) "Up",
    if' (i < n) (refPat (pths!!(i+1)) "Next") ""]
  where pths = nospace . title'' <$> ss
        (Just i) = elemIndex (nospace $ title'' s) pths
        n = length ss - 1
upPrevNext x y = error $ show x <> show y



-- Main workhorse function
sectionToHTMLrec:: Bool -> Map Text [(Text, Text)] -> Map Text Text
                        -> Maybe Section -> Section ->  IO ()
sectionToHTMLrec mkPdf bkLinks colorDict parent s  = do
    -- Recursively call sub-sections: do this first,   so errors are specific
    when (isSect s) $ do createDirectoryIfMissing True $ "site/" <> t
                         sequence_ $ sectionToHTMLrec mkPdf bkLinks colorDict
                                     (Just s) <$> sbody s
    -- Set up temporary directory and make a directory
    tmpdir <- (<> t) <$> getTemporaryDirectory
    createDirectoryIfMissing True tmpdir

    let tmppth = tmpdir <> "/" <> t'

    -- Assemble LaTeX file from all subsections
    writeFile tmppth $ unpack $ toLatex s parent
    let cptexcmd = "cp " <> tmppth <> " site/" <> t <> ".tex"
    system cptexcmd

    -- generate html and images
    let pdcmd = "pandoc -f latex -t html --toc --quiet --mathjax --citeproc \
      \--bibliography=bib/my.bib --csl=bib/ieee.csl --from latex+raw_tex \
      \--lua-filter=src/tikz-to-png.lua -s -o " <> html <> " " <> tmppth
    _ <- system pdcmd

    -- move generated figures to site/img/
    pngs <- filter (\p -> drop (length p - 4) p == ".png") <$> listDirectory "."
    forM_ pngs (\p -> renameFile p ("site/img/" <> p))

    -- If we also want pdfs
    when mkPdf (do
      let pdfcmd = "pdflatex -output-directory=" <> tmpdir <> " " <> tmppth
      system pdfcmd
      let cpcmd = "cp " <> (tmppth <> ".pdf") <> " site/" <> od
      system cpcmd >> pure ())

    -- Modify html code
    fixHtml (title'' s) bkLinks colorDict html Nothing


  where
    [t, t'] = unpack . toLower . nospace <$> ([title'', title'] <*> [s])
    html = "site/" <> t <> ".html"
    od = unpack $ intercalate "/" $ init $ splitOn "/" $ pack t

-- Top level call. Initialize things and cleanup after compiling pdfs
sectionToHTML:: Bool -> Section -> IO ()
sectionToHTML mkPdf topSection = do
    -- initialize site/img directory with img/ directory
    createDirectoryIfMissing True "site/img"
    imgs <- listDirectory "img"
    sequence_ $ (\x -> copyFile ("img/"<>x) ("site/img/"<>x)) <$> imgs
    -- copy toplevel files
    sequence_ $ (\x-> copyFile ("src/"<>x) ("site/"<> x)) <$> [
      "demo.css", "jquery.minipreview.js", "jquery.minipreview.css"]
    -- recursively add each section
    sectionToHTMLrec mkPdf bkLinks colorDict Nothing topSection
  where bkLinks = getInternalLinks topSection
        colorDict = getColors topSection
