{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text.Lazy
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
                           nbClick :: String,                           
                           nbNotes :: [Note]
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

instance ToJSON Note
instance FromJSON Note

instance ToJSON Result
instance FromJSON Result

instance Read Notebook

dbFile = "resources/nbs.json"

getNotebooks :: IO [Notebook]
getNotebooks = do
  putStrLn "Checking if database exists..."
  fEx <- doesFileExist dbFile
  if fEx then do
    nbstx <- readFile "resources/nbs.json"
    let nbs = (decode (BS.pack nbstx) :: Maybe [Notebook])
    case nbs of
      Nothing -> return []
      Just x  -> return x
  else
    return []

notebookExists :: Text -> IO Bool
notebookExists name = do
  nbs <- getNotebooks
  putStrLn "Notebooks retrieved"
  putStrLn $ show nbs
  let nbex = name `elem` (Prelude.map (pack . nbName) nbs)
  putStrLn $ show nbex
  return nbex

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
