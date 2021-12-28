module Main where
import System.Process (system)
import System.Environment (getArgs)
import Control.Monad (when)
--import Lib (getSections, sectionToHTML)
import ToHTML (makeSite, addHtml)
import DB (resetDB, loadDir, webc, normc, addURL, popWeb, toDB)
import ToLatex (addTeX)
import Text.RawString.QQ ( r )
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)

dsStore = [r|find doc -name .DS_Store -delete|]
clearCmd = [r|src/bash/clear.sh|]
clearCmd2 = [r|ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil|]
syncCmd = concat $ [r|rsync -r site/ ksb@rice.stanford.edu:afs-home/WWW/phil |]
                  : (f <$> [0..3])
  where f i = " --exclude '*._" <> show i <> ".html' "

flags = ["pdf", "sync", "clear", "gen"]

-- main2 :: IO ()
-- main2 = do
--   args <- getArgs
--   if not (all (`elem` flags) args)
--   then
--     putStrLn $ "valid flags " <> show flags
--   else do
--     system dsStore

--     when ("clear" `elem` args)
--       (system clearCmd >> system clearCmd2 >> pure ())

--     when ("gen" `elem` args) $ do
--       ss <- getSections "doc" -- parses doc/ folder
--       sectionToHTML ("pdf" `elem` args) ss -- modifies site/ folder

--     when ("sync" `elem` args)
--       (system syncCmd >> pure ())


sitePth :: FilePath
sitePth = "site2/"
srcPth = "doc"

main :: IO ()
main = do
  args <- getArgs
  if not (all (`elem` flags) args)
  then
    putStrLn $ "valid flags " <> show flags
  else do
    -- Fresh start: TODO remotely delete from rootURL
    when ("clear" `elem` args)
      (sequence_ [resetDB,
                  do b<- doesDirectoryExist sitePth
                     when b $ removeDirectoryRecursive sitePth])

    when ("gen" `elem` args) $ do
      -- Load data from files into memory and normalized db
      [c, c'] <- sequence [normc, webc]
      s <- loadDir srcPth
      toDB c s

      -- Populate web database
      popWeb c c'
      addURL c' "" s
      addTeX c' s
      addHtml c' sitePth s

      -- Populate site/ folder
      createDirectoryIfMissing True sitePth
      makeSite c' sitePth

    when ("sync" `elem` args) $ do
      -- Sync to web
      system $ "rsync -r " <> sitePth <> " ksb@rice.stanford.edu:afs-home/WWW/phil"
      pure ()
