module Main where
import System.Process (system)
import System.Environment (getArgs)
import Control.Monad (when)
import Lib (getSections, sectionToHTML)
import Text.RawString.QQ ( r )

dsStore = [r|find doc -name .DS_Store -delete|]
clearCmd = [r|src/clear.sh|]
clearCmd2 = [r|ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil|]
syncCmd = concat $ [r|rsync -r site/ ksb@rice.stanford.edu:afs-home/WWW/phil |]
                  : (f <$> [0..3])
  where f i = " --exclude '*._" <> show i <> ".html' "

flags = ["pdf", "sync", "clear", "gen"]

main :: IO ()
main = do
  args <- getArgs
  if not (all (`elem` flags) args)
  then
    putStrLn $ "valid flags " <> show flags
  else do
    system dsStore

    when ("clear" `elem` args)
      (system clearCmd >> system clearCmd2 >> pure ())

    when ("gen" `elem` args) $ do
      ss <- getSections "doc" -- parses doc/ folder
      sectionToHTML ("pdf" `elem` args) ss -- modifies site/ folder

    when ("sync" `elem` args)
      (system syncCmd >> pure ())
