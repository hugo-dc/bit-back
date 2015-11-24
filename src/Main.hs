{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text.Lazy
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           System.Directory
import           System.IO
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

data Note = Note { parentId :: Int,
                   ntId     :: Int,
                   ntYear   :: Int,
                   ntMonth  :: Int,
                   ntDay    :: Int,
                   nTitle   :: Int,
                   nContent :: String,
                   nHtml    :: String
                 } deriving (Show, Generic)

instance ToJSON Notebook
instance FromJSON Notebook

instance FromRow Notebook where
  fromRow = Notebook <$> field <*> field <*> field <*> field <*> field

instance ToRow Notebook where
  toRow (Notebook id name desc icon click) = toRow (name, desc, icon, click)

instance ToJSON Note
instance FromJSON Note

instance ToJSON Result
instance FromJSON Result

instance Read Notebook

dbFile = "resources/database"

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
    (Notebook 0 (unpack name) (unpack desc) "" "")

  close conn

createNotebook :: Text -> Text -> ActionM Result
createNotebook name desc = do
  nbEx <- liftIO (notebookExists name)
  if nbEx then 
    return $ Result False "Notebook already exists!"
  else do
    liftIO $ addNotebook name desc
    return $ Result True "Notebook created"

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/create-notebook/:name/:desc" $ do
      name <- param "name"
      desc <- param "desc"
      result <- createNotebook name desc
      json result
--      text ( "Creating note " <> name )
--    get "/users" $ do
--      json allUsers
--    get "/users/:id" $ do
--      id <- param "id"
--      json (filter (matchesId id) allUsers)

