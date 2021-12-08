
module Lib
  (sectionToHTML, getSections
  ) where
import Prelude hiding (concat, replicate)
import qualified Prelude as P
import Data.Text (Text, unpack, pack, splitOn, concat, replicate, intercalate,
                  replace, isInfixOf)
import qualified Data.Text as T
import System.Process (system)
import System.Directory (listDirectory, doesFileExist, copyFile,
                         createDirectoryIfMissing, renameFile, removeDirectory,
                         getTemporaryDirectory, withCurrentDirectory)
import Control.Monad (forM, forM_, when)
import Data.Either (partitionEithers)
import Data.Char(isSpace)
import Data.Map (Map)
import Data.List (sortOn, elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)
import Text.RawString.QQ ( r )

import FixHtml (fixHtml)
---------------------
-- Data structures --
---------------------
data TagType = Def | Prop | Exercise | Example deriving (Show, Eq, Ord, Read)
data MetaData = MetaData{ord::Maybe Float,
                         gist:: Maybe Text,
                         tag::Maybe TagType}
                          deriving (Show, Eq, Ord)

data Section = Sections {sname::Text, sintro::Text, sbody::[Section]}
             | Notes {nname::Text, nbody::Text}
             deriving (Show, Eq, Ord)



-- Parse the opening lines of a tex file that look like "% ORD 2" / "% TAG Def"
parseMetaData :: Text -> MetaData
parseMetaData t = MetaData (res "ORD") (pack <$> res' "GIST") (res "TAG")
  where tlines = fmap unpack <$> takeWhile (\l->not (T.null l) && (T.head l == '%')) $ T.lines t
        reg x = mkRegex ("\\% " <> x <> " (.*)")
        res' x = listToMaybe $ mapMaybe (fmap head . matchRegex (reg x)) tlines
        res x =  read <$> res' x

isSect :: Section -> Bool
isSect Sections {} = True
isSect _ = False

-- The path of the section, e.g. phil/Phil Quotes/Sellars/Scientium Mensura
title :: Section -> Text
title (Sections t _ _) = t
title (Notes t  _) = t

-- The title of the section
title' :: Section -> Text
title' = last . splitOn "/" . title

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
packages = ["tikz", "hyperref", "amsmath", "amssymb", "amsthm"]

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
        let intro = if' (null intros) "" (nbody $ head intros)
        let nonintros = sortOn (\s->(ord $ md s, title' s)) (filter (not . isIntro) children)
        return $ Sections (pack fp) intro nonintros)
   where
         isIntro x = title' x == last (splitOn "/" $ pack fp)


latexBody :: Int -> Section -> Text
latexBody _ (Notes _ n) = n
latexBody n (Sections _ i ss) = concat $ tc : i : map (f n) ss
 where tc = if' (n == 0) "\n\\tableofcontents\n" ""
       f n s = concat ["\n\\",hierarchy !! n, "{",
                       (nospace (title s) <> ".html") <> "|" <> title' s,
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
    pdflink = "\n" <> refPat (nospace $ title x <> ".pdf") "PDF"

-- Generate hyperlinks for the Up / Prev / Next buttons
upPrevNext :: Section -> Maybe Section -> Text
upPrevNext _ Nothing = ""
upPrevNext s (Just p@(Sections _ _ ss)) = intercalate "\n" [
    if' (i > 0) (refPat (pths!!(i-1) <> ".html") "Previous") "",
    refPat (nospace (title p) <> ".html") "Up",
    if' (i < n) (refPat (pths!!(i+1) <> ".html") "Next") ""]
  where pths = nospace . title <$> ss
        (Just i) = elemIndex (nospace $ title s) pths
        n = length ss - 1



-- Main workhorse function
sectionToHTMLrec:: Bool ->  Maybe Section -> Section ->  IO ()
sectionToHTMLrec mkPdf parent s  = do
  -- Set up temporary directory and make a directory in site/
  tmpdir <- (<> t) <$> getTemporaryDirectory
  createDirectoryIfMissing True tmpdir
  when (isSect s) (createDirectoryIfMissing True $ "site/" <> t)
  let tmppth = tmpdir <> "/" <> t'

  -- Assemble LaTeX file from all subsections
  writeFile tmppth $ unpack $ toLatex s parent
  let cptexcmd = "cp " <> tmppth <> " site/" <> t <> ".tex"
  system cptexcmd

  -- generate html and images
  let pdcmd = "pandoc -f latex -t html --toc --quiet --mathjax --citeproc \
    \--bibliography=my.bib --from latex+raw_tex \
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
  fixHtml html Nothing

  -- Recursively call sub-sections
  when (isSect s) (sequence_ $ sectionToHTMLrec mkPdf (Just s) <$> sbody s)

  where
    [t, t'] = unpack . nospace <$> ([title, title'] <*> [s])
    html = "site/" <> t <> ".html"
    od = unpack $ intercalate "/" $ init $ splitOn "/" $ pack t

-- Top level call. Initialize things and cleanup after compiling pdfs
sectionToHTML:: Bool -> Section -> IO ()
sectionToHTML mkPdf topSection  = do
  createDirectoryIfMissing True "site/img"
  sequence_ $ (\x-> copyFile ("src/"<>x) ("site/"<> x)) <$> [
    "demo.css", "jquery.minipreview.js", "jquery.minipreview.css"]
  sectionToHTMLrec mkPdf Nothing topSection
