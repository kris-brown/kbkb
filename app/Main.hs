module Main where
import System.Process (system)
import System.Environment (getArgs)
import Control.Monad (when)
import Lib (getSections, sectionToHTML)

-- Flags: pdf, sync, clear
main :: IO ()
main = do
    args <- getArgs
    ss <- getSections "doc"
    sectionToHTML ("pdf" `elem` args) ss
    when ("sync" `elem` args)
        (system "ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil" >> pure ())
    when ("clear" `elem` args)
        (system "rsync -rv site/ ksb@rice.stanford.edu:afs-home/WWW/phil" >> pure ())
