{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{- | Hastache templating for Scotty

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
module Main where

import Text.Hastache
import Web.Scotty.Trans as S
import Web.Scotty.Hastache

main :: IO ()
main = scottyH' 3000 $ do
  setTemplatesDir \"templates\"
  -- ^ Setting up the director with templates
  get \"/:word\" $ do
    beam <- param \"word\"
    setH \"action\" $ MuVariable (beam :: String)
    -- ^ \"action\" will be binded to the contents of \'beam\'
    hastache \"greet.html\"
@

Given the following template:

@
\<h1\>Scotty, {{action}} me up!\<\/h1\>
@

Upon the @GET \/beam@ the result will be:

@
\<h1\>Scotty, beam me up!\<\/h1\>
@

-}
module Web.Scotty.Hastache where

import           Data.Text.Lazy                  (Text)
import           Network.Wai                     (Application)
import           Network.Wai.Handler.Warp        (Port)
import           Text.Hastache

import qualified Web.Scotty.Hastache.Trans       as Trans
import           Web.Scotty.Trans                as S

-- * Runners and types

-- | The runner to use instead of 'scotty'
scottyH :: (ScottyError e) => Port -> ScottyH e () -> IO ()
scottyH p = Trans.scottyHT p id id

-- | The runner to use instead of 'scottyOpts'
scottyHOpts :: (ScottyError e) => Options -> ScottyH e () -> IO ()
scottyHOpts opts = Trans.scottyHOptsT opts id id

-- | A type synonym for @ScottyT e HState@; with custom exception types
type ScottyH e = Trans.ScottyHT e IO

-- | A type synonym for @ScottyT e HState@; with custom exception types
type ActionH e = Trans.ActionHT e IO

-- ** Specialized types and runners

type ScottyH' = ScottyH Text
type ActionH' = ActionH Text

scottyH' :: Port -> ScottyH' () -> IO ()
scottyH' = scottyH
scottyHOpts' :: Options -> ScottyH' () -> IO ()
scottyHOpts' = scottyHOpts

-- * The DSL itself

-- ** Configuration

-- | Update the Hastache configuration as whole
setHastacheConfig :: MuConfig IO -> ScottyH e ()
setHastacheConfig = Trans.setHastacheConfig

-- | Modify the Hastache configuration as whole
modifyHastacheConfig :: (MuConfig IO -> MuConfig IO) -> ScottyH e ()
modifyHastacheConfig = Trans.modifyHastacheConfig

-- | Set the path to the directory with templates. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplatesDir :: FilePath -> ScottyH e ()
setTemplatesDir = Trans.setTemplatesDir

-- | Set the default extension for template files. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplateFileExt :: String -> ScottyH e ()
setTemplateFileExt = Trans.setTemplateFileExt

-- ** Actions

-- | This is a function, just like 'S.html' or 'S.text'.
-- It takes a name of the template (the path is computed using the 
-- information about the templates dir and template files extension) 
-- and renders it using Hastache.
--
-- The variables that have been initialized using 'setH' are 
-- substituted for their values, uninitialized variables are 
-- considered to be empty/null.
hastache :: ScottyError e => FilePath -> ActionH e ()
hastache = Trans.hastache

-- | Set the value of a mustache variable.
setH :: ScottyError e => String -> MuType IO -> ActionH e ()
setH = Trans.setH

-- * Internals

scottyHApp :: MuConfig IO -> ScottyH e () -> IO Application
scottyHApp conf = Trans.scottyHAppT conf id id
