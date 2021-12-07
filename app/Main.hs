module Main where
import System.Process (system)
import Lib (getSections, sectionToHTML)

main :: IO ()
main = do
    ss <- getSections "doc"
    sectionToHTML False ss
    --system "ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil"
    --system "rsync -rv site/ ksb@rice.stanford.edu:afs-home/WWW/phil"
    return ()