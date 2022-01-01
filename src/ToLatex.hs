module ToLatex (toLatex , addTeX) where
import Data.Text (Text, isInfixOf, intercalate, unpack, pack,splitOn)
import qualified Data.Text as T
import DB
import System.Process (system)
import Text.RawString.QQ ( r )
import System.Directory (getTemporaryDirectory)
import Database.PostgreSQL.Simple (Connection, execute,query)
import Control.Monad (when, forM)

packages :: [Text]
packages = ["hyperref", "amsmath", "amssymb", "amsthm"]


preamb :: Text -> Text
preamb t = intercalate "\n" $ ["\\documentclass[12pt,a4paper]{report}"] ++ ((
  \p -> "\\usepackage{"<> p <> "}") <$> tik++packages) ++ [
    "\\begin{document}\n\\batchmode\n"]
  where tik = if' ("tikz" `isInfixOf` t) ["tikz", "tikz-cd"] []

-- Creating a latex body optimized for rendering to HTML
-- We'll want a different function if we want one that's optimized for PDF
latexBody :: Section -> IO Text
latexBody (Content Md n) = do
   tmpdir <- getTemporaryDirectory
   let [tmpmd, tmptex] = ((tmpdir <> "/tmp") <>) <$> ["md", "tex"]
   writeFile tmpmd $ unpack n
   system $ "pandoc -f markdown -t latex --quiet --mathjax -s -o " <> tmptex <> " " <> tmpmd
   res <- readFile tmptex
   if' (length res /= 1) (pure ()) (pure ())
   let [_,bodyEnd] = splitOn "\\begin{document}" $ pack res
   let [body,_] = splitOn "\\end{document}" bodyEnd

   return body
latexBody (Content Tex n) = pure n
latexBody (Sections (MData _ _ u) ss _) = do
  subsections <- forM ss latexBody
  return $ T.concat $ ["\n\n\n SECTIONSTART\n\n\n"] ++
                      subsections ++ ["\n\n\nSECTIONEND\n\n\n"]

fixCloze:: Text->Text
fixCloze t = T.concat $ fixCloze'  <$> zip [1..] (T.splitOn "\\," t)

fixCloze':: (Int, Text) -> Text
fixCloze' (i,t)
  | i == 1 = t
  | even i = "OPENCLOZE" <> t
  | otherwise = "CLOSECLOZE" <> t

toLatex :: Section -> Maybe Section -> IO Text
toLatex x parent = do
  lb <- latexBody x
  let hascite = "\\cite" `isInfixOf` lb
  let cit = if' hascite "\n\\bibliography{my}\n\\bibliographystyle{amsalpha}" ""
  return $ T.concat [preamb lb, "\n\\title{", title' x,
                    "}\n", fixCloze lb,cit, "\n\\end{document}"]

addTeX :: Connection -> Section -> IO ()
addTeX c Content {} = pure ()
addTeX c s@(Sections (MData ttl _ u) ss _) = do
  b <- query c "SELECT tex FROM section WHERE uuid=?;" [u] :: IO [[Maybe Text]]
  if' (b /= [[Nothing]]) (pure () -- print $ "Skipping tex generation for " <> ttl
    ) (do
    t <- toLatex s Nothing
    execute c q (t, u)
    sequence_ $ addTeX c <$> ss)
  where q = "UPDATE section SET tex = ? WHERE uuid = ?;"
