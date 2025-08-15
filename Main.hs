import Types (PageData(..), Item(..), Script(..))
import Layout (renderPageBody, layout)
import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension, takeDirectory)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (Text)
import Network.Wai.Parse (fileName, fileContent)
import Data.List (sort)
import Control.Applicative ((<|>))
import Data.Bool (bool)

filesDir :: FilePath
filesDir = "exercises"

getDirectoryData :: FilePath -> IO (Either Text (FilePath, [Item]))
getDirectoryData path = do
    let fullPath = filesDir </> path
    listResult <- try (listDirectory fullPath) :: IO (Either IOException [String])
    case listResult of
        Left _ -> return $ Left "Error: Directory not found or cannot be read."
        Right names -> do
            items <- traverse toItem (sort names)
            return $ Right (path, items)
    where
        toItem name = bool (FileItem name) (DirItem name) <$> doesDirectoryExist (filesDir </> path </> name)

getFileData :: FilePath -> IO (Either Text (FilePath, Text))
getFileData path = do
    let fullPath = filesDir </> path
    result <- try (BL.readFile fullPath) :: IO (Either IOException BL.ByteString)
    return $ case result of
        Left _ -> Left "Error: Archivo no encontrado."
        Right content -> Right (path, TLE.decodeUtf8 content)

main :: IO ()
main = scotty 3000 $ do
    get "/static/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/style.css"

    get "/" $ redirect "/browse/."

    get (regex "^/serve/(.*)$") $
        pathParam "1" >>= file . (filesDir </>) . TL.unpack

    get (regex "^/browse/(.*)$") $ do
        path <- TL.unpack <$> (pathParam "1" <|> pure "")
        let relativePath = if null path then "." else path
        result <- liftIO $ getDirectoryData relativePath
        either text (\(p, items) -> do
            let (body, scripts) = renderPageBody $ BrowserPage p items
            html $ layout "Navegador" scripts body
            ) result

    get (regex "^/exercise/(.*)$") $ do
        filepath <- pathParam "1"
        if takeExtension (TL.unpack filepath) `elem` [".html", ".htm"]
            then file (filesDir </> TL.unpack filepath)
            else do
                let parentDir = takeDirectory $ TL.unpack filepath
                dirResult <- liftIO $ getDirectoryData parentDir
                fileResult <- liftIO $ getFileData (TL.unpack filepath)
                let combinedResult = (FilePage . fst <$> fileResult) <*> (snd <$> fileResult) <*> (snd <$> dirResult)
                either text (\pageData -> do
                    let (body, scripts) = renderPageBody pageData
                    html $ layout ("Viendo: " <> filepath) scripts body
                    ) combinedResult

    post "/submit" $ do
        fs <- files
        currentDir <- formParam "current_dir"
        case fs of
            ((_, info):_) -> do
                let fname = T.unpack . TE.decodeUtf8 $ fileName info
                let saveDir = bool (filesDir </> TL.unpack currentDir) filesDir (currentDir == ".")
                liftIO $ BL.writeFile (saveDir </> fname) (fileContent info)
                redirect $ bool ("/browse/" <> currentDir) "/browse/." (currentDir == ".")
            [] -> redirect "/browse/."