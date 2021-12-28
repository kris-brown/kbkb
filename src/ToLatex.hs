module ToLatex (toLatex , addTeX) where
import Data.Text (Text, isInfixOf, intercalate, unpack)
import qualified Data.Text as T
import DB
import System.Process (system)
import Text.RawString.QQ ( r )
import Database.PostgreSQL.Simple (Connection, execute)

packages :: [Text]
packages = ["tikz", "tikz-cd", "hyperref", "amsmath", "amssymb", "amsthm"]


preamb :: Text
preamb = intercalate "\n" $ ["\\documentclass[12pt,a4paper]{report}"] ++ ((
  \p -> "\\usepackage{"<> p <> "}") <$> packages) ++ [
    "\\begin{document}\n"]

-- Creating a latex body optimized for rendering to HTML
-- We'll want a different function if we want one that's optimized for PDF
latexBody :: Int -> Section -> Text
latexBody _ (Content n) = n
latexBody n (Sections (MData _ _ u) ss _) = T.concat $
  ["\n\n\n SECTIONSTART\n\n\n"] ++ (f n <$> ss) ++ ["\n\n\nSECTIONEND\n\n\n"]
 where f n s =  latexBody (n+1) s


toLatex :: Section -> Maybe Section -> Text
toLatex x parent = T.concat [preamb, upprevnext, pdflink,
                    "\n\\title{", title' x,
                    "}\n", lb,cit, "\n\\end{document}"]
  where
    lb = latexBody 0 x
    hascite = "\\cite" `isInfixOf` lb
    cit = if' hascite "\n\\bibliography{my}\n\\bibliographystyle{amsalpha}" ""
    upprevnext = "" -- upPrevNext x parent
    pdflink = " " -- <> refPat (nospace $ title'' x <> ".pdf") "PDF"

addTeX :: Connection -> Section -> IO ()
addTeX c Content {} = pure ()
addTeX c s@(Sections (MData _ _ u) ss _) = do
  execute c q (toLatex s Nothing, u)
  sequence_ $ addTeX c <$> ss
    where q = "UPDATE section SET tex = ? WHERE uuid = ?;"

test = do s <- loadDir "bkup_doc"
          let res = unpack $ toLatex s Nothing
          writeFile "tst/test.tex" res
          system [r|pdflatex -output-directory=tst tst/test.tex|]
          system [r|open tst/test.pdf|]
