--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Monad           (void)
import           Data.Monoid             (mappend)
import           Data.String.Interpolate
import           Data.Time
import           Hakyll
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO


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
  let parseResult =
        execParserPure
          (prefs showHelpOnError)
          (info (helper <*> parser) fullDesc)
          args
  case parseResult of
    Success (New name) -> mkNewPost name >>= createNewPost
    Failure failure    -> do
      progname <- getProgName
      let (msg, _) = renderFailure failure progname
      putStrLn "CUSTOM COMMANDS"
      hPutStrLn stderr msg
      putStrLn "\nHAKYLL COMMANDS"
      hakyll rules
    completionInvoke   -> void $ handleParseResult completionInvoke
