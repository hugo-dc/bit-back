{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text, unpack)
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
--                       resultR   :: String,
                       messageR  :: String
                     } deriving (Show, Generic)

data Notebook = Notebook { nbId   :: Int,
                           nbName :: String,
                           nbDesc :: String,
                           nbIcon :: String,
                           nbClick :: String
--                           nbNotes :: [Note]
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

getNotebookId :: Text -> IO Integer
getNotebookId name = return 1

createDefaultNote :: Text -> IO ()
createDefaultNote name = do
  nbId  <- getNotebookId name
  defmd <- getDefaultMarkdown
  html  <- getHtml defmd
  conn  <- open dbFile
  (day, month, year) <- getDMY
  execute conn "INSERT INTO notes (parent, year, month, day, title, content, html) VALUES (?,?,?,?,?,?,?)" (Note 0 (fromIntegral nbId) year month (fromIntegral day) "Your first note" defmd html)
  close conn
  
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
  r <- query conn "SELECT * FROM notes WHERE id = ? AND parent = ?" [noteid, nbId] :: IO [Note]
  close conn
  if null r then
    error "Note not found!"
  else
    return $ head r          

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/create-notebook/:name/:desc" $ do
      name <- param "name"
      desc <- param "desc"
      result <- createNotebook name desc
      json result
    get "/get-note/:nbook/:noteid" $ do
      nbook  <- param "nbook"
      noteid <- param "noteid"
      note <- getNote nbook (read noteid :: Integer)
      json note
--      text ( "Creating note " <> name )
--    get "/users" $ do
--      json allUsers
--    get "/users/:id" $ do
--      id <- param "id"
--      json (filter (matchesId id) allUsers)

