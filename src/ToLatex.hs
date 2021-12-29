module ToLatex (toLatex , addTeX) where
import Data.Text (Text, isInfixOf, intercalate, unpack)
import qualified Data.Text as T
import DB
import System.Process (system)
import Text.RawString.QQ ( r )
import Database.PostgreSQL.Simple (Connection, execute,query)
import Control.Monad (when)
packages :: [Text]
packages = ["hyperref", "amsmath", "amssymb", "amsthm"]


preamb :: Text -> Text
preamb t = intercalate "\n" $ ["\\documentclass[12pt,a4paper]{report}"] ++ ((
  \p -> "\\usepackage{"<> p <> "}") <$> tik++packages) ++ [
    "\\begin{document}\n\\batchmode\n"]
  where tik = if' ("tikz" `isInfixOf` t) ["tikz", "tikz-cd"] []

-- Creating a latex body optimized for rendering to HTML
-- We'll want a different function if we want one that's optimized for PDF
latexBody :: Int -> Section -> Text
latexBody _ (Content n) = n
latexBody n (Sections (MData _ _ u) ss _) = T.concat $
  ["\n\n\n SECTIONSTART\n\n\n"] ++ (f n <$> ss) ++ ["\n\n\nSECTIONEND\n\n\n"]
 where f n s =  latexBody (n+1) s

fixCloze:: Text->Text
fixCloze t = T.concat $ fixCloze'  <$> zip [1..] (T.splitOn "\\," t)
fixCloze':: (Int, Text) -> Text
fixCloze' (i,t)
  | i == 1 = t
  | even i = "OPENCLOZE" <> t
  | otherwise = "CLOSECLOZE" <> t

toLatex :: Section -> Maybe Section -> Text
toLatex x parent = T.concat [preamb lb, upprevnext, pdflink,
                    "\n\\title{", title' x,
                    "}\n", fixCloze lb,cit, "\n\\end{document}"]
  where
    lb = latexBody 0 x
    hascite = "\\cite" `isInfixOf` lb
    cit = if' hascite "\n\\bibliography{my}\n\\bibliographystyle{amsalpha}" ""
    upprevnext = "" -- upPrevNext x parent
    pdflink = " " -- <> refPat (nospace $ title'' x <> ".pdf") "PDF"

addTeX :: Connection -> Section -> IO ()
addTeX c Content {} = pure ()
addTeX c s@(Sections (MData ttl _ u) ss _) = do
  b <- query c "SELECT tex FROM section WHERE uuid=?;" [u] :: IO [[Maybe Text]]
  if' (b /= [[Nothing]]) (print $ "Skipping tex generation for " <> ttl) (do
    let t = toLatex s Nothing
    --when (ttl == "Exercise 1-1") $ putStrLn $ unpack t
    execute c q (t, u)
    sequence_ $ addTeX c <$> ss)
  where q = "UPDATE section SET tex = ? WHERE uuid = ?;"

-- test = do s <- loadDir "bkup_doc"
--           let res = unpack $ toLatex s Nothing
--           writeFile "tst/test.tex" res
--           system [r|pdflatex -output-directory=tst tst/test.tex|]
--           system [r|open tst/test.pdf|]
