--------------------------------------------------------------------------------
{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------
import qualified Data.Map        as M
import           Data.Monoid     ((<>), mconcat)
import           Prelude         hiding (id)
import           System.FilePath

--------------------------------------------------------------------------------
import           Hakyll

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    -- Static files
    match statics $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler
    
    -- Render articles
    match "articles/*" $ do
        route   $ routeArticle
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/article.html" pageCtx
                >>= loadAndApplyTemplate "templates/default.html" pageCtx
                >>= relativizeUrls
    
    -- Article list
    create ["articles.html"] $ do
        route routePage
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let archiveCtx = mconcat
                           [ listField "articles" pageCtx (return articles)
                           , titleField "Articles"
                           , constField "pageTitle" "Blog | Benedict Eastaugh"
                           , constField "pageClass" "default"
                           , defaultContext ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    
    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- fmap (take 5) . recentFirst =<< loadAll "articles/*"
            let indexContext =
                    listField "articles" pageCtx (return articles) <>
                    constField "pageClass" "home" <>
                    constField "pageTitle" "Benedict Eastaugh" <>
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
    
    -- Read templates
    match "templates/*" $ compile $ templateCompiler
    
    -- Render some static pages
    match (fromList pages) $ do
        route   $ routePage
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
    
    -- Misc. pages
    match (fromList
                [ "research.md"
                , "talks.md"
                , "misc/coffee.md"
                , "misc/pkd.md"
                ]) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
    
    -- Projects page
    match   "projects.html" $ do
        route   $ routePage
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
    
    -- Render 404 page
    match "404.html" $ do
        route idRoute
        compile $ getResourceBody
            >>=  loadAndApplyTemplate "templates/default.html" pageCtx
    
    -- Render article feed
    create ["articles.atom"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "articles/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration "All articles") feedCtx

  where
    pages =
        [ "projects/hatt.md"
        , "projects/mobile.md"
        , "projects/papertrail.md"
        , "about.md"
        , "teaching.md"
        ]
    statics =
        ( "images/**"
     .||. "files/**"
     .||. "favicon.ico"
     .||. "robots.txt"
        )

--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , pageTitleCtx
    , constField "pageClass" "default"
    , defaultContext
    ]

--------------------------------------------------------------------------------
pageTitleCtx :: Context String
pageTitleCtx = field "pageTitle" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    let suffix = "Benedict Eastaugh"
    return $ fromMaybeF suffix ((++ " | " ++ suffix))
        (M.lookup "title" metadata)

--------------------------------------------------------------------------------
-- Article and page routing code

-- | Take a page like @\"/about/notebooks.md\"@ and route it to
-- @\"/about/notebooks\"@, i.e. turn a filename into a drectory.
--
routePage :: Routes
routePage = customRoute fileToDirectory

-- | Drop the date and set the file extension to ".html" when routing articles.
--
routeArticle :: Routes
routeArticle = routeArticleExt ".html"

-- | Drop the date and set the file extension to ".raw" when routing the raw
-- versions of articles.
--
routeArticleRaw :: Routes
routeArticleRaw = routeArticleExt ".txt"

-- | Article routing with a specific file extension.
--
routeArticleExt :: String -> Routes
routeArticleExt ext = customRoute
                    $ flip replaceExtension ext
                    . flip replaceDirectory "articles"
                    . dropDate

-- | Turn an @Identifier@ into a @FilePath@, dropping the date prefix (e.g.
-- @\"2011-04-07-\"@) along the way.
dropDate :: Identifier -> FilePath
dropDate ident = let file = toFilePath ident
                 in  replaceFileName file (drop 11 $ takeFileName file)

-- | Turn a filename reference into a directory with an index file.
--
fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -avzc -e ssh --delete _site/* \
                      \extralogical.net:\"/var/www/extralogical.net/public\""
    }

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Extralogical: " ++ title
    , feedDescription = "Logic and philosophy articles"
    , feedAuthorName  = "Benedict Eastaugh"
    , feedAuthorEmail = "benedict@eastaugh.net"
    , feedRoot        = "http://extralogical.net"
    }

--------------------------------------------------------------------------------
articleList :: ([Item String] -> Compiler [Item String]) -> Compiler String
articleList sortFilter = do
    articles <- sortFilter =<< loadAll "articles/*"
    itemTpl  <- loadBody "templates/article-item-long.html"
    list     <- applyTemplateList itemTpl pageCtx articles
    return list

--------------------------------------------------------------------------------
fromMaybeF :: a -> (a -> a) -> Maybe a -> a
fromMaybeF x _ Nothing  = x
fromMaybeF _ f (Just y) = f y
