module Main where
import System.Process (system)
import System.Environment (getArgs)
import Control.Monad (when)
import Lib (getSections, sectionToHTML, getInternalLinks)
import Text.RawString.QQ ( r )

dsStore = [r|find doc -name .DS_Store -delete|]
clearCmd = [r|src/clear.sh|]
clearCmd2 = [r|ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil|]

syncCmd = [r|rsync -rv site/ ksb@rice.stanford.edu:afs-home/WWW/phil --exclude '*._0.html' --exclude '*._1.html' --exclude '*._2.html'|]

-- Flags: pdf, sync, clear
main :: IO ()
main = do
  args <- getArgs

  system dsStore

  when ("clear" `elem` args)
    (system clearCmd >> system clearCmd2 >> pure ())

  ss <- getSections "doc"
  let lnks = getInternalLinks ss
  sectionToHTML ("pdf" `elem` args) lnks ss
  when ("sync" `elem` args)
    (system syncCmd >> pure ())
