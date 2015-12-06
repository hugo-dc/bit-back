{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text, unpack, pack)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           System.Directory
import           System.IO
import           System.Process
import           Web.Scotty

data Result = Result { successR  :: Bool,
                       messageR  :: String
                     } deriving (Show, Generic)

data Notebook = Notebook { nbId   :: Int,
                           nbName :: String,
                           nbDesc :: String,
                           nbIcon :: String,
                           nbClick :: String
                         } deriving (Show, Generic)

data Note = Note { ntId     :: Int,
                   parentId :: Int,
                   ntYear   :: Int,
                   ntMonth  :: Int,
                   ntDay    :: Int,
                   nTitle   :: String,
                   nContent :: String,
                   nHtml    :: String
                 } deriving (Show, Generic)

data DBInt = DBInt { dbInt :: Int } deriving (Show)

instance ToJSON Notebook
instance FromJSON Notebook

instance FromRow Notebook where
  fromRow = Notebook <$> field <*> field <*> field <*> field <*> field

instance ToRow Notebook where
  toRow (Notebook id name desc icon click) = toRow (name, desc, icon, click)

instance ToRow Note where
  toRow (Note id parent y m d title md html ) = toRow (parent, y,m,d,title,md,html)

instance FromRow Note where
  fromRow = Note <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow DBInt where
  fromRow = DBInt <$> field

instance ToJSON Note
instance FromJSON Note

instance ToJSON Result
instance FromJSON Result

instance Read Notebook

dbFile = "resources/database"
defMD  = "resources/default_note"

getDMY :: IO (Integer, Int, Int)
getDMY = getCurrentTime >>= return . toGregorian .utctDay

notebookExists :: Text -> IO Bool
notebookExists name = do
  conn <- open dbFile
  let stName = unpack name
  r <- query conn "SELECT name FROM notebooks WHERE name = ?" [(unpack name)] :: IO [[String]]
  let ex = (unpack name) `elem`  (Prelude.concat r)
  close conn
  return ex


addNotebook :: Text -> Text -> IO ()
addNotebook name desc = do
  conn <- open dbFile
  execute conn "INSERT INTO notebooks (name, description, icon, click) VALUES (?, ?, ?, ?)"
    (Notebook 0 (unpack name) (unpack desc) "fa-edit" ("openNotebook('" ++ (unpack name ) ++ "'"))
  close conn

getDefaultMarkdown :: IO String
getDefaultMarkdown = do
  md <- readFile defMD
  return md

getHtml :: String -> IO String
getHtml markdown = do
   (y,m,d) <- getDMY
   let fname = (show y) ++ "-" ++ (show m) ++ "-" ++ (show d)
   writeFile ("./bin/posts/" ++ fname ++ "-test.md") markdown
   cudir <- getCurrentDirectory
   callCommand (cudir ++ "\\bin\\build.bat" )
   html <- readFile (".\\bin\\_site\\posts\\" ++ fname ++ "-test.html")
   return html

safe :: ([t] -> a) -> [t] -> Maybe a
safe f [] = Nothing
safe f x = Just (f x)

getNotebookByName :: Text -> ActionM Notebook
getNotebookByName name = do
  nb <- liftIO (getNotebookByName' name)
  case nb of
    Nothing -> error "Notebook not found"
    Just n  -> return n

getNotebookByName' :: Text -> IO (Maybe Notebook)
getNotebookByName' name = do
  conn <- open dbFile
  r <- query conn "SELECT * FROM notebooks WHERE name = ?" [(unpack name)] :: IO [Notebook]
  return $ safe head r
  
getNotebookId :: Text -> IO Int
getNotebookId name = do
  nb <- getNotebookByName' name
  case nb of
    Nothing -> error "Notebook not found"
    Just n  -> return $  nbId n

createNote :: Integer -> Text -> Text -> ActionM Result
createNote nbid ntitle nmd = do
  liftIO $ createNote' nbid ntitle nmd
  return $ Result True "Note created!"

createNote' :: Integer -> Text -> Text -> IO ()
createNote' nbid ntitle nmd = do
  html <- getHtml (unpack nmd)
  conn <- open dbFile
  (year, month, day) <- getDMY
  execute conn "INSERT INTO notes (parent, year, month, day, title, content, html) VALUES (?,?,?,?,?,?,?)" (Note 0 (fromIntegral nbid) (fromIntegral year) month day (unpack ntitle) (unpack nmd) html)
  close conn

createDefaultNote :: Text -> IO ()
createDefaultNote name = do
  nbId  <- getNotebookId name
  defmd <- getDefaultMarkdown
  html  <- getHtml defmd
  createNote' (fromIntegral nbId) "Your First Note" (pack defmd)

  
createNotebook :: Text -> Text -> ActionM Result
createNotebook name desc = do
  nbEx <- liftIO (notebookExists name)
  if nbEx then 
    return $ Result False "Notebook already exists!"
  else do
    liftIO $ addNotebook name desc
    liftIO $ createDefaultNote name
    return $ Result True "Notebook created"

getNote :: Text -> Integer -> ActionM Note
getNote nbook noteid = do
  note <- liftIO (getNote' nbook noteid)
  return note

getNote' :: Text -> Integer -> IO Note
getNote' nbook noteid = do
  nbId <- getNotebookId nbook
  conn <- open dbFile
  r <- query conn "SELECT * FROM notes WHERE id = ? AND parent = ?" [(fromInteger noteid), nbId] :: IO [Note]
  close conn
  if null r then
    error "Note not found!"
  else
    return $ head r

getNotebooks :: ActionM [Notebook]
getNotebooks = do
   nbs <- liftIO getNotebooks'
   return nbs

getNotebooks' :: IO [Notebook]
getNotebooks' = do
   conn <- open dbFile
   r <- query_ conn "SELECT * FROM notebooks" :: IO [Notebook]
   close conn
   return r

getLastNote :: Int -> ActionM Note
getLastNote nbid = do
  note <- liftIO (getLastNote' nbid)
  return note

getLastNote' :: Int -> IO Note
getLastNote' nbid = do
  conn <- open dbFile
  c <- query conn "SELECT MAX (id) FROM notes WHERE parent = ?" [nbid :: Int] :: IO [DBInt]
  
  r <- query conn "SELECT * FROM notes WHERE id = ? AND parent = ? " [(dbInt . head) c , nbid] :: IO [Note]
  let note = safe head r
  case note of
    Nothing -> error "Note not found!"
    Just n  -> return n

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/api-ready" $ do
      json (Result True "API v0.1")
    get "/create-notebook/:name/:desc" $ do
      name <- param "name"
      desc <- param "desc"
      result <- createNotebook name desc
      json result
    get "/get-notebooks" $ do
      nbs <- getNotebooks
      json nbs
    get "/get-note/:nbook/:noteid" $ do
      nbook  <- param "nbook"
      noteid <- param "noteid"
      note <- getNote nbook (read noteid :: Integer)
      json note
    get "/get-notebook-by-name/:nbook" $ do
      name <- param "nbook"
      nbook <- getNotebookByName name
      json nbook
    get "/get-last-note/:nbid" $ do
      nbid <- param "nbid"
      note <- getLastNote nbid
      json note
    get "/create-note/:nbid/:ntitle/:nmd" $ do
      nbid   <- param "nbid"
      ntitle <- param "ntitle"
      nmd    <- param "nmd"
      result <- createNote nbid ntitle nmd
      json result
--      text ( "Creating note " <> name )
--    get "/users" $ do
--      json allUsers
--    get "/users/:id" $ do
--      id <- param "id"
--      json (filter (matchesId id) allUsers)

