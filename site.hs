--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid (mconcat,(<>))
import           Data.Function (on)
import           Data.List (sortBy,intersperse,intercalate,isInfixOf)
import           Data.List.Split (chunksOf)
import qualified Data.Map as M
import           Hakyll
import           System.FilePath (dropExtension,takeBaseName)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze ((!), toValue) 
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------

articlesPerIndexPage :: Int
articlesPerIndexPage = 2


hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
  deployCommand =
     "rsync -ave ssh _site/ " ++ "ben@benkolera.com:/opt/blog/"
  }
             
main :: IO ()
main = doHakyll

doHakyll :: IO ()
doHakyll = hakyllWith hakyllConf $ do
  tags <- buildTags postsPattern (fromCapture "blog/tags/*.html")
  pctx <- postCtx tags

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "talks/*" $ do
    route $ setExtension "html"
    compile $ templatedPandoc "templates/talk.html" "navTalks"

  match "posts/*/*/*/*.markdown" $ do
    route   $ postsRoute `composeRoutes` (setExtension ".html")
    compile $ postCompiler pctx

  match "posts/*/*/*/*/text.markdown" $ do
    route   $ postsRoute `composeRoutes` gsubRoute "text.markdown" (const "index.html")
    compile $ postCompiler pctx

  match "posts/*/*/*/*/*" $ do
    route   postsRoute
    compile copyFileCompiler


  match "talks.html" $ do
    route idRoute
    compile $ listCompiler baseCtx "talks/*" "content" "talks" "Talks"

  match "archives.html" $ do
    route idRoute
    compile $ listCompiler baseCtx "posts/*" "content" "posts" "Archives"      

  match ("static/*.markdown" .||. "static/**/*.markdown") $ do
    route $ gsubRoute "static/" (const "") `composeRoutes` setExtension ".html" 
    compile staticCompiler
    
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler


  mds <- getAllMetadata postsPattern
  let
    ids = reverse $ map fst $ sortBy (compare `on` ((M.! "published") . snd)) mds
    pids = chunksOf articlesPerIndexPage ids
    indexPages = map pagePath [1..length pids]
    pagePath i =
      fromFilePath $ concat ["blog/index",if i == 1 then "" else show i,".html"]
    indexes = zip indexPages pids


    
  create (map fst indexes) $ do
    route idRoute
    compile $ indexCompiler tags indexes (constField "navPosts" "true")
    
  match "css/*.less" $ do
    compile getResourceBody

  d <- makePatternDependency "css/**/*.less"
  rulesExtraDependencies [d] $ create ["css/blog.css"] $ do
    route idRoute
    compile $ loadBody "css/blog.less"
      >>= makeItem
      >>= withItemBody
      (unixFilter "node_modules/less/bin/lessc" ["-","--include-path=css/"])
      
  where
    postsPattern = fromGlob "posts/*/*/*/*.markdown" .||.
                   fromGlob "posts/*/*/*/*/text.markdown"
    postsRoute   = gsubRoute "posts/" (const "blog/posts/")
        
--------------------------------------------------------------------------------

templatedPandoc :: Identifier -> String -> Compiler (Item String)
templatedPandoc templatePath navName = pandocCompiler
  >>= loadAndApplyTemplate templatePath ctx
  >>= saveSnapshot "content"
  >>= defaultCompiler ctx
  where
    ctx = (constField navName "true" <> baseCtx)

staticCompiler :: Compiler (Item String)
staticCompiler = pandocCompiler >>= defaultCompiler baseCtx
    
defaultCompiler :: Show a => Context a -> Item a -> Compiler (Item String)
defaultCompiler ctx item =
  loadAndApplyTemplate "templates/default.html" ctx item
  >>= relativizeUrls

listCompiler :: 
  Context String ->
  Pattern ->
  String ->
  String ->
  String ->
  Compiler (Item String)
listCompiler ctx  matcher snapshotName fieldName title = do
  list <- recentFirst =<< loadAllSnapshots matcher snapshotName
  let listCtx = mconcat [
        listField fieldName ctx (return list)
        , constField "title" title     
        , ctx
        ]
  getResourceBody >>= applyAsTemplate listCtx >>= defaultCompiler listCtx


postCtx :: MonadMetadata m => Tags -> m (Context String)
postCtx t = 
  return $ mconcat [
    mapContext prettifyTag (tagsFieldWith getTags renderTag join "prettytags" t)
    , tagCloudCtx t
    , functionField "readmore" readMoreField
    , constField "navPosts" "true"
    , baseCtx
  ]
  where
    prettifyTag "" = ""
    prettifyTag s = "<div class=\"tags\">" ++ s ++ "</div>"
    renderTag _ Nothing = Nothing
    renderTag tag (Just filePath) = Just $
      H.span ! A.class_ "tag" $ 
      H.a ! A.href (toValue $ toUrl filePath) $
      H.toHtml tag
    join = mconcat . intersperse " "


baseCtx :: Context String
baseCtx =
  mconcat [
    functionField "pagetitle" pageTitle
    , dateField "date" "%B %e, %Y" 
    , defaultContext
    ]
  where
    pageTitle _ i = do
      m <- getMetadata $ itemIdentifier i
      return $ "Confessions of a Typeholic" ++ (maybe "" (" | "++ ) (M.lookup "title" m))

readMoreField :: [String] -> Item String -> Compiler String
readMoreField _ i = do
  rte <- getRoute $ itemIdentifier i
  return $ case rte of
    Nothing -> ""
    Just r -> if isInfixOf "<!--MORE-->" (itemBody i)
              then readMoreLink r
              else ""
    where readMoreLink r' =
            renderHtml $ H.div ! A.class_ "readmore" $
            H.a ! A.href (toValue $ "/" ++ r') $
            H.preEscapedToMarkup ("Read more &raquo;"::String)

tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudFieldWith "tagcloud" makeLink (intercalate " ") 100 200
  where
    makeLink minSize maxSize tag url count min' max' = renderHtml $
        H.span ! A.class_ "tagcloud" !
        A.style (toValue $ "font-size: " ++ size count min' max') $
        H.a ! A.href (toValue url) $ H.toHtml tag
      where
        -- Show the relative size of one 'count' in percent
        size count' min'' max'' =
          let diff = 1 + fromIntegral max'' - fromIntegral min''
              relative = (fromIntegral count' - fromIntegral min'') / diff
              size' = floor $ minSize + relative * (maxSize - minSize)
          in show (size' :: Int) ++ "%"

indexCompiler ::
  Tags ->
  [(Identifier, [Identifier])] ->
  Context String ->
  Compiler (Item String)
indexCompiler tags ids extraCtx = do
  pg <- (drop 5 . dropExtension . takeBaseName . toFilePath) <$> getUnderlying
  let i = if pg == "" then 1 else (read pg :: Int)
      n = length ids
      older = indexNavLink i 1 n
      newer = indexNavLink i (-1) n
  list <- postList (fromList $ snd $ ids !! (i - 1)) recentFirst
  makeItem ""
    >>= loadAndApplyTemplates ["posts","default"] (
      mconcat [
        constField "title" "Blog Index"
        , constField "posts" list
        , constField "navlinkolder" older
        , constField "navlinknewer" newer
        , tagCloudCtx tags
        , extraCtx
        , baseCtx
        ]
      )

indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxN = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $
                   (H.preEscapedToMarkup lab)
        lab :: String
        lab = if (d > 0) then "Older Posts" else "Newer Posts"
        refPage = pagePath (n + d)
        pagePath 1                       = "/blog/index.html"
        pagePath p | (p < 1 || p > maxN) = ""
        pagePath p                       = "/blog/index" ++ (show p) ++ ".html"

loadAndApplyTemplates ::
  [String] ->
  Context String ->
  Item String ->
  Compiler (Item String)
loadAndApplyTemplates [c] ctx i =
  loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
loadAndApplyTemplates (c:cs) ctx i = do
  i' <- loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
  loadAndApplyTemplates cs ctx i'

postList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList postPattern postProcess = do
  posts <- loadAllSnapshots postPattern "content" >>= postProcess
  return $ concatMap itemBody posts

postCompiler :: Context String -> Compiler (Item String)
postCompiler defCtx = do
  pandocCompiler
    >>= saveSnapshot "post" 
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= saveSnapshot "content"
    >>= defaultCompiler ctx
  where
    ctx = (constField "navPosts" "true" <> defCtx)
