{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.Scotty.Hastache.Trans where

import           Control.Arrow                   ((***))
import           Control.Monad.State             as State
import           Data.Default (def)
import           Data.IORef                      (newIORef, readIORef,
                                                  writeIORef)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (mempty)
import           Network.Wai                     (Application, Response)
import           Network.Wai.Handler.Warp        (Port, setPort)
import           System.FilePath.Posix           ((</>))
import           Text.Blaze.Html.Renderer.String as BRS
import           Text.Blaze.Html.Renderer.Utf8   as BRU
import           Text.Blaze.Internal             (Markup)
import           Text.Hastache
import           Text.Hastache.Context

import           Web.Scotty.Trans                as S

-- * Runners and types

-- | The runner to use instead of 'scottyT'
scottyHT :: (ScottyError e, MonadIO m, MonadIO n) => Port -> (forall a. m a -> n a) -> (m Response -> IO Response) -> ScottyHT e m () -> n ()
scottyHT p = scottyHOptsT $ def { settings = setPort p (settings def) }

-- | The runner to use instead of 'scottyOptsT'
scottyHOptsT :: (ScottyError e, MonadIO m, MonadIO n) => Options -> (forall a. m a -> n a) -> (m Response -> IO Response) -> ScottyHT e m () -> n ()
scottyHOptsT opts runM runActionToIO s = do
    (runHT, runActionToM) <- runM $ mkHStateTRunners defaultConfig
    scottyOptsT opts (runM . runHT) (runActionToIO . runActionToM) s

-- | A type synonym for @ScottyT e (HStateT m)@; with custom exception types
type ScottyHT e m = ScottyT e (HStateT m)

-- | A type synonym for @ScottyT e (HStateT m)@; with custom exception types
type ActionHT e m = ActionT e (HStateT m)

-- * The DSL itself

-- ** Configuration

-- | Update the Hastache configuration as whole
setHastacheConfig :: Monad m => MuConfig m -> ScottyHT e m  ()
setHastacheConfig conf = do
  (_, tmap) <- lift State.get
  lift . State.put $ (conf, tmap)

-- | Modify the Hastache configuration as whole
modifyHastacheConfig :: Monad m => (MuConfig m -> MuConfig m) -> ScottyHT e m ()
modifyHastacheConfig f = lift $ State.modify (f *** id)

-- | Set the path to the directory with templates. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplatesDir :: Monad m => FilePath -> ScottyHT e m ()
setTemplatesDir dir = do
  lift $ State.modify $ \(conf :: MuConfig m, tmap) ->
      (conf { muTemplateFileDir = Just dir }, tmap)

-- | Set the default extension for template files. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplateFileExt :: Monad m => String -> ScottyHT e m ()
setTemplateFileExt ext = do
  lift $ State.modify $ \(conf :: MuConfig m, tmap) ->
      (conf { muTemplateFileExt = Just ext }, tmap)

-- ** Actions

-- | This is a function, just like 'S.html' or 'S.text'.
-- It takes a name of the template (the path is computed using the
-- information about the templates dir and template files extension)
-- and renders it using Hastache.
--
-- The variables that have been initialized using 'setH' are
-- substituted for their values, uninitialized variables are
-- considered to be empty/null.
hastache :: (ScottyError e, MonadIO m) => FilePath -> ActionHT e m ()
hastache tpl = do
  ((conf :: MuConfig m), tmap) <- lift State.get
  setHeader "Content-Type" "text/html"
  let cntx a  = fromMaybe MuNothing (M.lookup a tmap)
  let tplFile = fromMaybe "." (muTemplateFileDir conf)
              </> tpl
              ++ fromMaybe "" (muTemplateFileExt conf)
  res <- lift . lift $ hastacheFile conf tplFile (mkStrContext cntx)
  raw res

-- | Set the value of a mustache variable.
setH :: (ScottyError e, Monad m) => String -> MuType m -> ActionHT e m ()
setH x y = do
  (conf, tmap) <- lift State.get
  lift . State.put $ (conf, M.insert x y tmap)

-- * Internals

-- | State with the Hastache config
type HStateT m = StateT (MuConfig m, M.Map String (MuType m)) m

mkHStateTRunners :: MonadIO m => MuConfig m -> m (forall a. HStateT m a -> m a, HStateT m Response -> m Response)
mkHStateTRunners conf = do
    gstate <- liftIO $ newIORef undefined
    let runHT m = do
            (r,(muconf,_)) <- runStateT m (conf, mempty)
            liftIO $ writeIORef gstate muconf
            return r
        runActionToM m = do
            muconf <- liftIO $ readIORef gstate
            evalStateT m (muconf, mempty)
    return (runHT, runActionToM)

scottyHAppT :: (MonadIO m, MonadIO n) => MuConfig m -> (forall a. m a -> n a) -> (m Response -> IO Response) -> ScottyHT e m () -> n Application
scottyHAppT conf runM runActionToIO defs = do
    (runHT, runActionToM) <- runM $ mkHStateTRunners conf
    scottyAppT (runM . runHT) (runActionToIO . runActionToM) defs

instance Show Markup where
  show = BRS.renderHtml

instance MuVar Markup where
  isEmpty = isEmpty . BRU.renderHtml
  toLByteString = BRU.renderHtml


