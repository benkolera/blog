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

import Debug.Trace

-------------------------------------------------------------------------------

debug s = trace ("STUFF: " ++ show s) s

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
  match "static/**" $ do
    route   $ gsubRoute "static/" (const "")
    compile copyFileCompiler    

  match ("md/pages/**.md") $ do
    route $ gsubRoute "md/pages/" (const "") `composeRoutes` setExtension ".html"
    compile staticCompiler

  match "templates/*" $ compile templateCompiler

  -- Build tags
  tags <- buildTags "md/posts/*" (fromCapture "tags/*.html")

  -- Render each and every post
  match "md/posts/*.md" $ do
    route $ gsubRoute "md/" (const "") `composeRoutes` setExtension ".html"
    compile $ templatedPandoc "templates/post.html" tags

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "md/posts/**" "content"
      let ctx = listField "posts" (postCtx tags) (return . debug $ posts) <> baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "less/*.less" $ compile getResourceBody

  d <- makePatternDependency "less/**/*.less"
  rulesExtraDependencies [d] $ create ["css/blog.css"] $ do
    route idRoute
    compile $ loadBody "less/blog.less"
      >>= makeItem
      >>= withItemBody
      (unixFilter "node_modules/less/bin/lessc" ["-","--include-path=less/"])
      
--------------------------------------------------------------------------------

templatedPandoc :: Identifier -> Tags -> Compiler (Item String)
templatedPandoc templatePath tags = pandocCompiler
  >>= saveSnapshot "content"
  >>= loadAndApplyTemplate templatePath (postCtx tags)
  >>= defaultCompiler baseCtx

staticCompiler :: Compiler (Item String)
staticCompiler = pandocCompiler >>= defaultCompiler baseCtx

defaultCompiler :: Show a => Context a -> Item a -> Compiler (Item String)
defaultCompiler ctx item =
  loadAndApplyTemplate "templates/default.html" ctx item
  >>= relativizeUrls

postCtx tags = mconcat
  [ modificationTimeField "mtime" "%U"
  , tagsField "tags" tags
  , baseCtx
  ]

baseCtx :: Context String
baseCtx =
  mconcat [
    functionField "pagetitle" pageTitle
    , dateField "today" "%B %e, %Y"
    , defaultContext
    ]
  where
    pageTitle _ i = do
      m <- getMetadata $ itemIdentifier i
      return $ "Confessions of a Typeholic" ++ (maybe "" (" | "++ ) (M.lookup "title" m))
