import           Data.Either (fromRight)
import           Data.Maybe (fromMaybe)
import           Data.Void (Void)
import           System.FilePath (takeBaseName)
import qualified Text.Megaparsec as P
import           Text.Megaparsec.Char (char)
import qualified Hakyll
import           Hakyll hiding (PlainText, defaultContext)

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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= minify

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
                >>= minify


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
                >>= minify

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

defaultContext :: Context String
defaultContext = plainTitle <> htmlTitle <> Hakyll.defaultContext

plainTitle :: Context String
plainTitle = field "title" $ \item -> do
    let identifier = itemIdentifier item
    metadata <- getMetadata identifier
    return $ fromMaybe
        (takeBaseName $ toFilePath identifier)
        (titleToPlainText <$> lookupString "title" metadata)

htmlTitle :: Context String
htmlTitle = field "html-title" $ \item -> do
    let identifier = itemIdentifier item
    metadata <- getMetadata identifier
    return $ fromMaybe
        (takeBaseName $ toFilePath identifier)
        (titleToHtml <$> lookupString "title" metadata)

titleToPlainText :: String -> String
titleToPlainText title = concatMap
    (\case
        PlainText s -> s
        Code s      -> s)
    (parseTitle title)

titleToHtml :: String -> String
titleToHtml title = concatMap
    (\case
        PlainText s -> s
        Code s      -> "<code>" ++ s ++ "</code>")
    (parseTitle title)

data TitlePart = PlainText String | Code String deriving Show
type Title = [TitlePart]
type Parser = P.Parsec Void String

parseTitle :: String -> Title
parseTitle title = fromRight [] $
    P.parse (P.many (P.try parseTitlePlainText P.<|> parseTitleCode)) "" title

parseTitlePlainText :: Parser TitlePart
parseTitlePlainText = PlainText <$>
    P.takeWhile1P (Just "Plain Text Character") (/= '`')

parseTitleCode :: Parser TitlePart
parseTitleCode = Code <$>
    (char '`' *> P.manyTill (P.label "Code Character" P.anySingle) (char '`'))

minify :: Item String -> Compiler (Item String)
minify = withItemBody $ unixFilter "tidy" tidyOptions

tidyOptions :: [String]
tidyOptions =
    [ "-quiet"
    , "-utf8"
    , "-ashtml"
    , "--tidy-mark", "no"
    ]
