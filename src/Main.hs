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
         fromRow = Notebook <$> field <*> field

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
  r <- query conn "SELECT * FROM notebooks WHERE name = ?" [(unpack name)] :: IO [Notebook]
  let ex = (unpack name) `elem` (Prelude.map nbName r)
  close conn
  return ex


addNotebook :: Text -> IO ()
addNotebook name = do
  conn <- open dbFile
  execute conn "INSERT INTO notebooks (name) VALUES (?)"
    (Only name)
  close conn

createNotebook :: Text -> ActionM Result
createNotebook name = do
  nbEx <- liftIO (notebookExists name)
  if nbEx then 
    return $ Result False "Notebook already exists!"
  else do
    liftIO $ addNotebook name 
    return $ Result True "Notebook created"

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/create-notebook/:name" $ do
      name <- param "name"
      result <- createNotebook name
      json result
--      text ( "Creating note " <> name )
--    get "/users" $ do
--      json allUsers
--    get "/users/:id" $ do
--      id <- param "id"
--      json (filter (matchesId id) allUsers)
