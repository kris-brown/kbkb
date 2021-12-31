module Main where
import System.Process (system)
import System.Environment (getArgs)
import Control.Monad (when, unless)
import ToHTML (makeSite, addHtml)
import DB (MData (..), Section (..), resetDB, loadDir, webc, normc,
           addURL, popWeb, toDB, popNumChildren)
import ToLatex (addTeX)
import Text.RawString.QQ ( r )
import System.Directory (createDirectoryIfMissing,
                         removeDirectoryRecursive, doesDirectoryExist)

flags = ["pdf", "sync", "clear", "gen", "clear_remote", "clear_db"]
sitePth = "site/"
srcPth = "doc"
dsStore = "find "<> srcPth <> " -name .DS_Store -delete"
clearCmd2 = [r|ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil|]
syncCmd = "rsync -r " <> sitePth <>
          " ksb@rice.stanford.edu:afs-home/WWW/phil"

main :: IO ()
main = do
  args <- getArgs
  if not (all (`elem` flags) args)
  then
    putStrLn $ "valid flags " <> show flags
  else do

    system dsStore

    -- Fresh start
    when ("clear_remote" `elem` args) $ system clearCmd2 >> pure ()
    when ("clear_db" `elem` args) $ resetDB
    when ("clear" `elem` args) (do b <- doesDirectoryExist sitePth
                                   when b $ removeDirectoryRecursive sitePth)

    when ("gen" `elem` args) $ do
      -- Load data from files into memory and normalized db
      [c, c'] <- sequence [normc, webc]
      s <- loadDir srcPth
      unless ("root" == uid (mdata s)) $ error "\n\nDID NOT POINT TO ROOT\n\n"
      toDB c s
      popNumChildren c

      -- Populate web database
      popWeb c c'
      addURL c' "" s
      addTeX c' s
      addHtml c' sitePth s

      -- Populate site/ folder
      createDirectoryIfMissing True sitePth
      makeSite c' sitePth

    when ("sync" `elem` args) $ system syncCmd >> pure ()

