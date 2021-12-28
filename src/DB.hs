module DB
  (Section(..), TagType(..), MData(..), if' , title', loadDir, url, normc, webc,
   toDB, addURL, popWeb, resetDB, isSections ) where
import Data.Tuple.Extra (uncurry3)
import Data.Hashable ( Hashable, hash )
import Text.Read (readMaybe)
import Data.List (sort)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Text ( Text, pack, unpack, splitOn, breakOn, intercalate)
import Control.Monad (when, unless, forM, forM_)
import Data.Char (isSpace, isAlphaNum, isDigit)
import Database.PostgreSQL.Simple.Types ( Query(Query))
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple
import qualified Data.ByteString.UTF8 as BSU
import Database.PostgreSQL.Simple ( connect, defaultConnectInfo,
      execute_, execute, executeMany, query, query_,
      ConnectInfo(..),Connection )
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)

import System.Directory ( doesFileExist, listDirectory, createDirectory,
                          removeDirectoryRecursive, doesPathExist )

-- CONSTANTS
------------
mdataFile :: FilePath
mdataFile = "0"

commFile :: FilePath
commFile = "comm.csv"

-- Simple helpers
-----------------
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

strip::Text->Text
strip = T.filter p . f
  where p x = isAlphaNum x || x == '-'
        f =  T.replace " " "-"

dropDigitSpace :: String -> String
dropDigitSpace = dropWhile (\x->isDigit x || isSpace x)

-- Basic DB
-----------
conn :: String -> IO Connection
conn t = connect defaultConnectInfo{
        connectHost = "localhost",
        connectDatabase = t,
        connectUser = "ksb",
        connectPassword = ""}
normc = conn "kbkb"
webc = conn "web"

resetDB :: IO ()
resetDB = do c <- webc
             c' <- normc
             resetCmd <- B.readFile "src/misc/create_web.sql"
             resetCmd' <- B.readFile "src/misc/create_norm.sql"
             execute_ c $ Query resetCmd
             execute_ c' $ Query resetCmd'
             pure ()

-- Data structures
------------------

data TagType = Kris | Def | Prop | Exercise | Example | Default
             deriving (Show, Eq, Ord, Read, Generic)
data Comment = Comment {email::Text, tstamp::Text, body::Text}
             deriving (Show, Eq, Ord, Generic)
data MData = MData {title::Text, tag::TagType, uid::Text}
                deriving (Show, Eq, Ord, Generic)
data Section = Sections {mdata::MData, children::[Section], comments::[Comment]}
            | Content Text deriving (Show, Eq, Ord, Generic)

instance Hashable TagType
instance Hashable Comment
instance Hashable MData
instance Hashable Section

title' = title . mdata
isSections Sections {} = True
isSections Content {} = False

-- Printing/Parsing
-------------------

-- Parse CSV file naively with '|' delimiter
parseComments :: String -> [Comment]
parseComments = fmap (uncurry3 Comment . f . pack) . lines
  where f l = let [a,b,c] = splitOn "|" l in (a,b,c)

-- Inverse of parseComments
writeComments :: [Comment] -> String
writeComments =
  unlines . fmap (\(Comment a b c)-> unpack $ intercalate "|" [a,b,c])

-- Parse metadata from file. Default title/uid from filepath
parseMData :: FilePath -> String -> MData
parseMData fp s = MData a'' b (pack c)
  where dtitle = unpack $ last $ splitOn "/" $ pack fp
        duid = unpack $ T.replace "/" "-" $ T.filter (not . isSpace) $ pack fp
        a' = pack $ dropDigitSpace a
        a'' = if' (T.null a') (error fp) a'
        (a,b,c) = case lines s of
            [] -> (dtitle, Default, duid)
            [x] -> (x, Default, duid)
            [x, y] -> case readMaybe y of
                        Nothing -> (x, Default, y)
                        Just q -> (x, q, duid)
            [x, y, z] -> (x, read y, z)
            _ -> error $ "Metadata file too long " <> s

-- inverse of parseMdata
writeMData :: MData -> String
writeMData (MData a b c) = unlines $ unpack <$> [a, pack $ show b, c]

-- Parse the body of an internal \ref{...}
parseRef :: Text -> (Text, Text, Text)
parseRef t = case splitOn "|" t of
  [x] -> (x, "", "")
  [x,y] -> (x,y,"")
  [x,y,z] -> (x,y,z)
  _ -> error $ unpack t

-- Reading/writing to filesystem
--------------------------------
-- Load a directory
loadDir :: FilePath -> IO Section
loadDir fp = do files <- filter (`notElem` ps) . sort <$> listDirectory fp
                [m, c] <- sequence $ doesFileExist <$> ps'
                mdata <- parseMData fp <$> if' m (readFile mpth) (pure "")
                cdata <- parseComments <$> if' c (readFile cpth) (pure "")
                contents <- sequence $ loadDirElement fp <$> files
                return $ Sections mdata contents cdata
    where ps = [mdataFile, commFile]
          ps'@[mpth, cpth] = ((fp <> "/") <>) <$> ps

-- Process files and directories differently
loadDirElement :: FilePath -> String -> IO Section
loadDirElement fp s = do bool <- doesFileExist pth
                         if' bool loadTxt loadDir pth
  where pth = fp <> "/" <> s
        loadTxt = fmap (Content . pack) . readFile

-- Write Sections to directories
writeSection :: Section -> FilePath -> IO ()
writeSection s@(Sections md ss cm) fp = do
    putStrLn $ "writing md " <> show md <> "\n\t to " <> fp
    putStrLn $ "creating " <> newdir
    createDirectory newdir
    writeFile (newdir <> commFile) $ writeComments cm
    writeFile (newdir <> mdataFile) $ writeMData md
    forM_ (zip [1..] ss) f
  where dname = unpack $ strip $ title $ md
        newdir = fp <> dname <> "/"
        f (i, (Content n)) = writeFile (newdir <> show i <> "Content.tex") $ unpack n
        f (i, s) = writeSection s (newdir <> show i)
writeSection _ _ = error ""

-- Reading/writing to database
------------------------------
-- Top-level call to write to database
toDB :: Connection -> Section -> IO ()
toDB c s = toDB'' c (hash s) (1, s)

contentToDB:: Connection -> Int -> (Bool, Int, Text) -> IO ()
contentToDB c contId (isHead, ord, txt) = do
    unless (T.null t1) (do
        [[i']] <- cid o1 :: IO [[Int]]
        execute c q'' (i', u, d, cm)
        pure ())
    [[i]] <- cid (o1+1) :: IO [[Int]]
    execute c q' (i, removeBracket t2)
    pure ()
  where cid o = query c q (contId, o)
        o1 = 2*(ord-1)
        q = "INSERT INTO content (cont, ord) VALUES (?, ?) RETURNING id;"
        q' = "INSERT INTO latex (cont, val) VALUES (?,?)"
        q'' = "INSERT INTO intlink (cont, uuid, display, comm) VALUES (?,?,?,?)"
        (u, d, cm) = parseRef $ T.tail t1
        (t1, t2) = if' isHead ("", txt) (breakOn "}" txt)
        removeBracket = if' isHead id T.tail


toDB' :: Connection -> Section -> IO ()
toDB' c s@(Content n) = do
    execute c "INSERT INTO contents (id, sect) VALUES (?,?)" [hash n, hash s]
    sequence_  $ contentToDB c (hash n) <$> contentArgs
  where contentArgs = zip3 ((==1) <$> [1..]) [1..] (splitOn "\\ref" n)
toDB' c s@(Sections m b cmt) = do
    execute c q (hash s, hash s, show $ tag m, title m, uid m)
    executeMany c q2 $ cmts <$> cmt
    sequence_ $ toDB'' c (hash s) <$> zip [1..] b
  where
    q = "INSERT INTO sections (id, sect, tag, title, uuid) VALUES (?,?,?,?,?)"
    q2 = "INSERT INTO comments (sect, email, tstamp, body) VALUES (?,?,?,?)"
    cmts (Comment x y z) = (hash s, x, y, z)

toDB'' :: Connection -> Int -> (Int,Section) -> IO ()
toDB'' c parentId (ordId, s) = do b <- checkId c (hash s) "section"
                                  when b $ do
                                     execute c q (hash s, parentId, ordId)
                                     toDB' c s
    where q = "INSERT INTO section (id, parent, ord) VALUES (?,?,?)"

-- Get whether a particular table has a particular primary key
checkId :: Connection -> Int -> String -> IO Bool
checkId c i s = null <$> (query c q [i] :: IO [[Int]])
  where q = Query $ "SELECT 1 FROM " <> BSU.fromString s <> " WHERE id = ?"


-- Get the top-level section from DB
fromDB :: Connection -> IO Section
fromDB c = do [[i]] <- query_ c "SELECT id FROM section WHERE id=parent"
              fromDB' c i

contentFromDB :: Connection -> Int -> IO Text
contentFromDB c contID = do
  [r1, r2] <- sequence [q, q']
  case (r1, r2) of
    ([[j]],[]) -> head . head <$> query c vq [j]
    ([], [[j]]) -> do [[x,y,z]] <- query c lq [j]
                      return $ T.concat ["\\ref{",intercalate "|" [x,y,z] ,"}"]
    z -> error $ show z
  where
    q = query c "SELECT id FROM latex WHERE cont = ?" [contID] :: IO [[Int]]
    q' = query c "SELECT id FROM intlink WHERE cont = ?" [contID] :: IO [[Int]]
    vq = "SELECT val FROM latex WHERE id = ?"
    lq = "SELECT uuid,display,comm FROM intlink WHERE id = ?"

-- Assuming int is ID of a section that is content, get the text
contentsFromDB :: Connection -> Int -> IO Section
contentsFromDB c sectID = do
    [[contID]] <- query c qc [sectID] :: IO [[Int]]
    xs <- fmap head <$> q contID
    ys <- sequence $ contentFromDB c <$> xs
    return $ Content $ T.concat ys
  where qc = "SELECT id FROM contents WHERE sect=?"
        q i = query c "SELECT id FROM content WHERE cont=? ORDER BY ord" [i]
-- Get section with ID i
fromDB' :: Connection -> Int -> IO Section
fromDB' c i = do
  ss <- q
  cc <- q'
  case (ss,cc) of
    ([m], []) -> do cs <- fmap (uncurry3 Comment) <$> query c qc [i]
                    children_ids <- fmap head <$> query c qch [i,i]
                    ch <- sequence $ fromDB' c <$> children_ids
                    return $ Sections m ch cs -- Sections md bs cs
    ([], [[_]])  -> contentsFromDB c i  -- Content
    z -> error $ show (i, z)
  where
    q = fmap f <$> query c qmd [i]
    qmd = "SELECT title, tag::text, uuid FROM sections WHERE sect=?"
    q' = query c "SELECT 1 FROM contents WHERE sect=?" [i] :: IO [[Int]]
    f [a,b,c] = MData a (read $ unpack b) c
    qc = "SELECT email,tstamp,body FROM comments WHERE sect = ? ORDER BY tstamp"
    qch = "SELECT id FROM section WHERE parent=? AND id<>? ORDER BY ord"

url :: Connection -> Section -> IO Text
url c s = head . head <$> query c q [uid $ mdata s]
  where q = "SELECT urlpth FROM section WHERE uuid=?"

-- Load content from denormalized database into web database
popWeb :: Connection -> Connection -> IO ()
popWeb dConn wConn = do
    sectData <- query_ dConn q1 :: IO [(Int,Int,Int,Text,Text)]
    executeMany wConn q2 sectData
    linkData <- query_ dConn q3 :: IO [(Text,Text,Text,Text)]
    -- Get IDs for
    linkData' <- sequence $ getId <$> linkData
    executeMany wConn q4 linkData'
    pure ()
  where q1 = "SELECT section.id,parent,ord,uuid,title \
             \FROM section JOIN sections USING (id)"
        q2 = "INSERT INTO section (id,parent,ord,uuid,title) VALUES (?,?,?,?,?)"
        q3 = "SELECT sections.uuid,intlink.uuid,display,comm \
              \FROM intlink JOIN content ON (intlink.cont = content.id) \
              \ JOIN contents ON (content.cont = contents.id) \
              \ JOIN section ON (contents.sect = section.id) \
              \ JOIN sections ON (section.parent = sections.sect);"
        q4 = "INSERT INTO link (src,tgt,display,comm,repl) VALUES (?,?,?,?,?)"
        q5 = "SELECT id FROM section WHERE uuid=?"
        getId tup@(u1,u2,x2,x3) = do
          -- pattern match failure here IF we have broken internal link
          [[i1]] <- query wConn q5 [u1] :: IO [[Int]]
          i2' <- query wConn q5 [u2] :: IO [[Int]]
          let i2 = case i2' of
                        [[i]] -> i
                        _ -> error $ show tup
          return (i1,i2, x2, x3, intercalate "|" [u2,x2,x3])

addURL :: Connection -> Text -> Section -> IO ()
addURL _ _ Content {} = pure ()
addURL c prevURL s@(Sections (MData _ _ u) ss _) = do
  execute c q (newURL, u)
  sequence_ $ addURL c newURL <$> ss
    where q = "UPDATE section SET urlpth = ? WHERE uuid = ?;"
          newURL = prevURL <> "/" <> u

test :: IO Section
test = do resetDB
          c <- normc
          putStrLn "loading bkup_doc"
          s <- loadDir "bkup_doc"

          putStrLn "inserting loaded bkup_doc into DB"
          toDB c s

          putStrLn "read from DB and check unchanged"
          s' <- fromDB c
          when (s /= s') $ error "not equal"

          b <- doesPathExist tst
          putStrLn $  "creating new dir already exists: " <> show b
          when b $ removeDirectoryRecursive tst
          createDirectory tst
          putStrLn "writing to new dir"
          writeSection s tst
          putStrLn "loading from new dir"
          s'' <- loadDir $ tst <> "KBKB"
          putStrLn "removing new dir and recreating"
          removeDirectoryRecursive $ tst <> "KBKB"
          writeSection s'' tst
          s''' <- loadDir $ tst <> "KBKB"
          when (s'' /= s''') $ error "not equal"
          return s
  where tst = "tst/"
