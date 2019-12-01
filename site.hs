import Data.Maybe (catMaybes)
import System.FilePath (takeBaseName)
import Hakyll hiding (defaultContext)
import qualified Hakyll

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , storeDirectory       = ".hakyll/store"
    , tmpDirectory         = ".hakyll/tmp"
    }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

defaultContext :: Context String
defaultContext = htmlTitle <> Hakyll.defaultContext

htmlTitle :: Context String
htmlTitle = field "html-title" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let possibleHtmlTitles = [
            lookupString "html-title" metadata,
            lookupString "title" metadata,
            Just $ takeBaseName $ toFilePath $ itemIdentifier item]
    return $ head $ catMaybes possibleHtmlTitles
