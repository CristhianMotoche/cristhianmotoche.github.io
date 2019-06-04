--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Data.Monoid (mappend)
import           System.Directory
import           System.FilePath
import           Options.Applicative
import           Hakyll
import           System.Environment
import           Data.Time
import           Data.String.Interpolate


--
data Opts = New String deriving Show


-- |
parser :: Parser Opts
parser =
  New <$> subparser
    (command "new" (info (option str (long "name")) (progDesc "New post")))

-- |
data Post = Post { name :: String, date :: UTCTime } deriving Show

mkNewPost :: String -> IO Post
mkNewPost name_ = Post name_ <$> getCurrentTime

createNewPost :: Post -> IO ()
createNewPost post@Post{..} =
  let dateFile = formatTime defaultTimeLocale "%Y-%m-%d" date
      filename = dateFile ++ "-" ++ replace ' ' '-' name ++ ".md"
      filepath = "posts" </> filename
      replace _ _ "" = ""
      replace fromChar toChar (x:xs) =
        if x == fromChar
           then toChar:replace fromChar toChar xs
           else x:replace fromChar toChar xs
  in do
    exists <- doesFileExist filepath
    if exists
       then return ()
       else writeFile filepath (genTemplate post)

genTemplate :: Post -> String
genTemplate Post{..} = [i|---
title: #{name}
date: #{formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date}
tags: pending
description: Pending
---
# Title

Content
|]

-- |
rules :: Rules ()
rules = do
    match "images/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "sitemap.xml" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler


-- |
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


-- |
main :: IO ()
main = do
  args <- getArgs
  let x = execParserPure defaultPrefs (info parser fullDesc) args
  case getParseResult x of
    Just (New name) -> mkNewPost name >>= createNewPost
    _ -> hakyll rules
