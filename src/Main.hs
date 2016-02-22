{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text, unpack, pack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           Web.Scotty


-- Data
data DBTable = DBTable {
       dbTable :: String
  }
  
data Result = Result {
  successR  :: Bool,
  messageR  :: String
  } deriving (Show, Generic)

data Notebook = Notebook {
  nbId   :: Int,
  nbName :: String,
  nbDesc :: String
  } deriving (Show, Generic)

data Note = Note {
  ntId     :: Int,
  parentId :: Int,
  ntYear   :: Int,
  ntMonth  :: Int,
  ntDay    :: Int,
  nTitle   :: String,
  nContent :: String
  } deriving (Show, Generic)


data CrNote  = CrNote {
  crNbId    :: Integer,
  crTitle   :: String,
  crContent :: String
  } deriving (Show, Generic)

data UpdNote = UpdNote {
  updId    :: Integer,
  updTitle :: String,
  updContent :: String
  } deriving (Show, Generic)

data Favorite = Favorite {
  fvId :: Int,
  noteId :: Int
  } deriving (Show, Generic)

data DBInt = DBInt {
  dbInt :: Int
  } deriving (Show)

data DBStr = DBStr {
  dbStr :: String
  } deriving (Show)


-- Instances

instance ToJSON Notebook
instance FromJSON Notebook

instance FromJSON UpdNote
instance FromJSON CrNote

instance ToJSON Note
instance FromJSON Note

instance ToJSON Result
instance FromJSON Result

instance ToJSON Favorite

instance FromRow Notebook where
  fromRow = Notebook <$> field <*> field <*> field

instance ToRow Notebook where
  toRow (Notebook id name desc) = toRow (name, desc)

instance ToRow Note where
  toRow (Note id parent y m d title cont) = toRow (parent, y,m,d,title,cont)

instance FromRow Note where
  fromRow = Note <$> field <*> field <*> field <*> field <*> field <*> field <*> field 

instance FromRow Favorite where
  fromRow = Favorite <$> field <*> field 
  
instance ToRow Favorite where
  toRow (Favorite id note) = toRow [note]

instance FromRow DBInt where
  fromRow = DBInt <$> field

instance FromRow DBStr where
  fromRow = DBStr <$> field

instance FromRow DBTable where
  fromRow = DBTable <$> field 

instance Read Notebook

-- Functions

-- | SQLite database
dbFile = "resources/bitacorapp.db"

-- | Default note file
defMD  = "resources/default_note"

-- | Get current timestamp
getDMY :: IO (Integer, Int, Int)
getDMY = getCurrentTime >>= return . toGregorian .utctDay

-- | Checks note existence
notebookExists :: Text -> IO Bool
notebookExists name = do
  conn <- open dbFile
  let stName = unpack name
  r <- query conn "SELECT name FROM notebooks WHERE name = ?" [(unpack name)] :: IO [[String]]
  let ex = (unpack name) `elem`  (Prelude.concat r)
  close conn
  return ex

-- | Create new Notebook
addNotebook :: Text -> Text -> IO ()
addNotebook name desc = do
  conn <- open dbFile
  execute conn "INSERT INTO notebooks (name, description) VALUES (?, ?)"
    (Notebook 0 (unpack name) (unpack desc))
  close conn

-- | Get default note's markdown
getDefaultNote :: IO String
getDefaultNote = do
  md <- readFile defMD
  return md

-- | Safe f
safe :: ([t] -> a) -> [t] -> Maybe a
safe f [] = Nothing
safe f x = Just (f x)

-- | Return a Notebook based on its name
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

-- | Get Note Id based on its name  
getNotebookId :: Text -> IO Int
getNotebookId name = do
  nb <- getNotebookByName' name
  case nb of
    Nothing -> error "Notebook not found"
    Just n  -> return $  nbId n
    
-- | Update Note
updateNote :: Integer -> String -> String -> ActionM Result
updateNote ntid ntitle nmd = do
  liftIO $ updateNote' ntid ntitle nmd
  return $ Result True "Note updated!"

updateNote' :: Integer -> String -> String -> IO ()
updateNote' ntid ntitle nmd = do
  conn <- open dbFile
  execute conn "UPDATE notes SET title = ? WHERE id = ?"   (ntitle, ntid)
  execute conn "UPDATE notes SET content = ? WHERE id = ?" (nmd, ntid)
  close conn

-- | Delete Note
deleteNote :: Integer -> ActionM Result
deleteNote ntid = do
  liftIO $ deleteNote' ntid
  return $ Result True "Note deleted!"

deleteNote' :: Integer -> IO ()
deleteNote' ntid = do
-- | We need if note exists before deleting  
  conn <- open dbFile
  r <- query conn "DELETE FROM notes WHERE id = ?" [(ntid)] :: IO [[Integer]]
  putStrLn $ show r
  close conn
-- We need to check if note exists after deleting  


-- | Favorite Note
favNote :: Integer -> ActionM Result
favNote ntid = do
  liftIO $ favNote' ntid
  return $ Result True "Note is in favorites now"

favNote' :: Integer -> IO ()
favNote' ntid = do
  conn <- open dbFile
  execute conn "INSERT INTO favorites (note) VALUES (?)" (Favorite 0 (fromIntegral ntid))
  close conn

-- | Remove note from favorites
unfavNote :: Integer -> ActionM Result
unfavNote ntid = do
  liftIO $ unfavNote' ntid
  return $ Result True "Note is not favorite"

unfavNote' :: Integer -> IO ()
unfavNote' ntid = do
  conn <- open dbFile
  r <- query conn "DELETE FROM favorites WHERE note = ?" [(ntid)] :: IO [[Integer]]
  close conn

-- | Get Favorite Notes
getFavorites :: ActionM [Favorite]
getFavorites = do
  res <- liftIO getFavorites'
  return res

getFavorites' :: IO [Favorite]
getFavorites' = do
  conn <- open dbFile
  r <- query
         conn
         "SELECT * FROM favorites"
         () :: IO [Favorite]
  close conn
  if null r then
    return []
  else do
    return r
  
-- | Checks if note is in favorites
isFav :: Integer -> ActionM Result
isFav ntid = do
  fav <- liftIO $ isFav' ntid
  case fav of
    True  -> return $ Result True  "Note is favorite"
    False -> return $ Result False "Note is not favorite"

isFav' :: Integer -> IO Bool
isFav' ntid = do
  conn <- open dbFile
  r <- query
         conn
         "SELECT * FROM favorites WHERE note = ?"
         [ntid] :: IO [Favorite]
  close conn
  if null r then
    return False
  else return True
  
-- | Create Note  
createNote :: Integer -> String -> String -> ActionM Result
createNote nbid ntitle nmd = do
  liftIO $ createNote' nbid ntitle nmd
  return $ Result True "Note created!"

createNote' :: Integer -> String -> String -> IO ()
createNote' nbid ntitle content = do
  conn <- open dbFile
  (year, month, day) <- getDMY
  execute conn "INSERT INTO notes (parent, year, month, day, title, content) VALUES (?,?,?,?,?,?)" (Note 0 (fromIntegral nbid) (fromIntegral year) month day ntitle content)
  close conn

-- | Create default Note
createDefaultNote :: Text -> IO ()
createDefaultNote name = do
  nbId  <- getNotebookId name
  defmd <- getDefaultNote
  createNote' (fromIntegral nbId) "Your First Note" defmd

-- | Create Notebook  
createNotebook :: Text -> Text -> ActionM Result
createNotebook name desc = do
  nbEx <- liftIO (notebookExists name)
  if nbEx then 
    return $ Result False "Notebook already exists!"
  else do
    liftIO $ addNotebook name desc
    liftIO $ createDefaultNote name
    return $ Result True "Notebook created"

-- | Get Previous Note
getPrevNote :: Integer -> Integer -> ActionM Note
getPrevNote nbid ntid = do
  note <- liftIO (getPrevNote' nbid ntid)
  return note

getPrevNote' :: Integer -> Integer -> IO Note
getPrevNote' nbid ntid = do
  conn <- open dbFile
  c <- query conn "SELECT MAX(id) FROM notes WHERE id < ? AND parent = ?" [ntid, nbid] :: IO [DBInt]
  let nid = (dbInt . head) c
  note <- getNote' (toInteger nid)
  return note

-- | Get Next Note
getNextNote :: Integer -> Integer -> ActionM Note
getNextNote nbid ntid = do
  note <- liftIO (getNextNote' nbid ntid)
  return note

getNextNote' :: Integer -> Integer -> IO Note
getNextNote' nbid ntid = do
  conn <- open dbFile
  c <- query conn "SELECT id FROM notes WHERE id > ? AND parent = ?" [ntid, nbid] :: IO [DBInt]
  let nid = (dbInt . head ) c
  note <- getNote' (toInteger nid)
  return note

-- | Get Note using Note ID
getNote :: Integer -> ActionM Note
getNote ntid = do
  note <- liftIO (getNote' ntid)
  return note

getNote' :: Integer -> IO Note
getNote' ntid = do
  let nt = fromInteger ntid :: Int
  conn <- open dbFile
  r <- query
         conn
         "SELECT * FROM notes WHERE id = ?"
         [nt] :: IO [Note]
  close conn
  if null r then
    error "Note not found!"
  else
    return $ head r

-- | Get Notebooks
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

-- | Get Last Note
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

-- | Get years of all notes
getYears :: Integer -> ActionM [Int]
getYears nbid = do
  years <- liftIO $ getYears' nbid
  return years

getYears' :: Integer -> IO [Int]
getYears' nbid = do
  conn <- open dbFile
  c <- query conn "SELECT DISTINCT year FROM notes WHERE parent = ?" [(fromInteger nbid :: Int)] :: IO [DBInt]
  close conn
  return $ map dbInt c

-- | Get all months for the current year
getMonths :: Integer -> Integer -> ActionM [Int]
getMonths nbid year = do
  months <- liftIO $ getMonths' nbid year
  return months

getMonths' :: Integer -> Integer -> IO [Int]
getMonths' nbid year = do
  conn <- open dbFile
  c <- query conn "SELECT DISTINCT month FROM notes WHERE parent = ? AND year = ?" [(fromInteger nbid :: Int), (fromInteger year :: Int)] :: IO [DBInt]
  close conn
  return $ map dbInt c

-- | Get days of the current Month/Year
getDays :: Integer -> Integer -> Integer -> ActionM [Int]
getDays nbid year month = do
  days <- liftIO $ getDays' nbid year month
  return days

getDays' :: Integer -> Integer -> Integer -> IO [Int]
getDays' nbid year month = do
  conn <- open dbFile
  c <- query conn "SELECT DISTINCT day FROM notes WHERE parent = ? AND year = ? AND month = ?" [(fromInteger nbid :: Int), (fromInteger year :: Int), (fromInteger month :: Int)] :: IO [DBInt]
  close conn
  return $ map dbInt c

-- | Get all the notes for all Year/Month/Day
getNotesByDay :: Integer -> Integer -> Integer -> Integer -> ActionM [Note]
getNotesByDay nbid year month day = do
  notes <- liftIO $ getNotesByDay' nbid year month day
  return notes

getNotesByDay' :: Integer -> Integer -> Integer -> Integer -> IO [Note]
getNotesByDay' nbid year month day = do
  conn <- open dbFile
  c <- query
       conn
       "SELECT * FROM notes WHERE parent = ? AND year = ? AND month = ? AND day = ?" 
       [(fromInteger nbid :: Int)
       ,(fromInteger year :: Int)
       ,(fromInteger month :: Int)
       ,(fromInteger day   :: Int)]
       :: IO [Note]
  close conn
  return c


-- | Get Table definition
createTab :: String -> Connection -> IO ()
createTab t conn
  | t  == "notebooks" = do
      execute_
        conn
        "CREATE TABLE notebooks ( id INTEGER PRIMARY KEY, name TEXT, description TEXT)"
  | t == "notes"      = do
      execute_
        conn
        "CREATE TABLE notes ( id INTEGER PRIMARY KEY, parent NUMERIC, year NUMERIC, month NUMERIC, day NUMERIC, title TEXT, content TEXT)"
  | t == "favorites"  = do
      execute_
        conn
        "CREATE TABLE favorites ( id INTEGER PRIMARY KEY, note NUMERIC )"

-- | Check single table
checkTable :: String -> ActionM ()
checkTable tname = do
  liftIO $ checkTable' tname

checkTable' :: String -> IO ()
checkTable' tname = do
  conn <- open dbFile
  c <- query
       conn
       "SELECT name FROM sqlite_master WHERE type = ? AND name = ?"
       ["table" :: String,
        tname   :: String ] :: IO [DBTable]

  if null c then do
    createTab tname conn
  else return ()
  close conn    

-- | Check tables in DB
checkTables :: ActionM ()
checkTables = do
  checkTable "notebooks"
  checkTable "notes"
  checkTable "favorites"
  

-- | Checks system
startup :: ActionM Result
startup = do
  res <- checkTables
  return $ (Result True "API v0.2")

--------------------------------
dev = True

getPort :: Int
getPort 
  | dev       = 3001
  | otherwise = 3000

-- Main Function
main :: IO ()
main = do
  putStrLn "Starting server..."
  
  let port = getPort
  
  scotty port $ do
    get "/api-ready" $ do
      res <- startup
      json res

    get "/create-notebook/:name/:desc" $ do
      name <- param "name"
      desc <- param "desc"
      result <- createNotebook name desc
      json result
    get "/get-notebooks" $ do
      nbs <- getNotebooks
      json nbs
    get "/get-prev/:nbid/:ntid" $ do
      nbid <- param "nbid"
      ntid <- param "ntid"
      note <- getPrevNote (read nbid :: Integer) ntid
      json note
    get "/get-next/:nbid/:ntid" $ do
      nbid <- param "nbid"
      ntid <- param "ntid"
      note <- getNextNote (read nbid :: Integer) ntid
      json note
    get "/get-note/:ntid" $ do
      ntid <- param "ntid"
      note <- getNote (read ntid :: Integer )
      json note
    get "/get-notebook-by-name/:nbook" $ do
      name <- param "nbook"
      nbook <- getNotebookByName name
      json nbook
    get "/get-last-note/:nbid" $ do
      nbid <- param "nbid"
      note <- getLastNote nbid
      json note
    post "/create-note" $ do
      d <- body
      liftIO $ putStrLn (show d)
      let n = decode d :: Maybe CrNote
      case n of
        Just dt -> do
          r <- createNote (crNbId dt) (crTitle dt) (crContent dt)
          json r
        _ -> json $ Result False (show d) --"Note cannot be created"

    post "/update-note" $ do
      d <- body
      let n = decode d :: Maybe UpdNote
      case n of
        Just dt -> do
          r <- updateNote (updId dt) (updTitle dt) (updContent dt)
          json r
        _       -> json $ Result False "Note cannot be updated"

    get "/delete-note/:ntid" $ do
      ntid <- param "ntid"
      res <- deleteNote ntid
      json res
    get "/fav-note/:ntid" $ do
      ntid <- param "ntid"
      res <- favNote ntid
      json res
    get "/unfav-note/:ntid" $ do
      ntid <- param "ntid"
      res <- unfavNote ntid
      json res
    get "/get-favs" $ do
      res <- getFavorites
      json res
    get "/is-fav/:ntid" $ do
      ntid <- param "ntid"
      res <- isFav ntid
      json res
    get "/get-years/:nbid" $ do
      nbid <- param "nbid"
      years <- getYears nbid
      json years
    get "/get-months/:nbid/:year" $ do
      nbid <- param "nbid"
      year <- param "year"
      months <- getMonths nbid year
      json months
    get "/get-days/:nbid/:year/:month" $ do
      nbid <- param "nbid"
      year <- param "year"
      month <- param "month"
      days <- getDays nbid year month
      json days
    get "/get-notes-by-day/:nbid/:year/:month/:day" $ do
      nbid  <- param "nbid"
      year  <- param "year"
      month <- param "month"
      day   <- param "day"
      notes <- getNotesByDay nbid year month day
      json notes

