import Types ( PageData(FilePage, BrowserPage), Item(..) )
import Layout ( renderPageBody, layout )
import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>), (<$>))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai.Parse (fileName, fileContent)
import Data.Bool

filesDir :: FilePath
filesDir = "exercises"

-- I/O
getDirectoryData :: FilePath -> IO (Either Text (FilePath, [Item]))
getDirectoryData path = do
    let fullPath = filesDir </> path
    result <- try (listDirectory fullPath) :: IO (Either IOException [String])
    case result of
        Left _ -> return $ Left "Error: Directorio no encontrado o no se puede leer."
        Right names -> do
            items <- forM (sort names) (\name -> 
                do
                isDir <- doesDirectoryExist (fullPath </> name)
                pure (bool (FileItem name) (DirItem name) isDir))
            return (Right (path, items))

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

    get (regex "^/serve/(.*)$") $ do
        filepath <- TL.unpack <$> pathParam "1"
        file (filesDir </> filepath)

    get (regex "^/browse/(.*)$") $ do
        path <- TL.unpack <$> (pathParam "1" <|> pure "")
        let relativePath = if null path then "." else path
        result <- liftIO $ getDirectoryData relativePath
        either text (\(p, items) -> html $ layout "Navegador" (renderPageBody $ BrowserPage p items)) result

    get (regex "^/exercise/(.*)$") $ do
        filepath <- pathParam "1"
        if takeExtension (TL.unpack filepath) `elem` [".html", ".htm"]
            then file (filesDir </> TL.unpack filepath)
            else do
                let parentDir = takeDirectory $ TL.unpack filepath
                dirResult <- liftIO $ getDirectoryData parentDir
                fileResult <- liftIO $ getFileData (TL.unpack filepath)
                let combinedResult = (FilePage . fst <$> fileResult) <*> (snd <$> fileResult) <*> (snd <$> dirResult)
                either text (html . layout ("Viendo: " <> filepath) . renderPageBody) combinedResult

    post "/submit" $ do
        fs <- files
        currentDir <- formParam "current_dir"
        case fs of
            ((_, info):_) -> do
                let fname = T.unpack . TE.decodeUtf8 $ fileName info
                let saveDir = if currentDir == "." then filesDir else filesDir </> TL.unpack currentDir
                liftIO $ BL.writeFile (saveDir </> fname) (fileContent info)
                redirect $ if currentDir == "." then "/browse/." else "/browse/" <> currentDir
            [] -> redirect "/browse/."