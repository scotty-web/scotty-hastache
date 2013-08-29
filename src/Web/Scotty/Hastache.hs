{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ImpredicativeTypes         #-}

-- | Hastache templating for Scotty
module Web.Scotty.Hastache where

import           Control.Monad.State             as State
import qualified Data.Map                        as M
import           Data.Maybe
import    Data.IORef
import           Data.Monoid
import           Network.Wai
import           Network.Wai.Handler.Warp        (Port)
import           System.FilePath.Posix
import           Text.Blaze.Html.Renderer.String as BRS
import           Text.Blaze.Html.Renderer.Utf8   as BRU
import           Text.Blaze.Internal
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty.Trans                as S

-- | State with Hastache config
type HState = StateT ((MuConfig IO, M.Map String (MuType IO))) IO

type ScottyH = ScottyT HState
type ActionH = ActionT HState

mkHStateRunners :: MuConfig IO -> IO (forall a. HState a -> IO a, HState Response -> IO Response)
mkHStateRunners conf = do
    gstate <- newIORef undefined
    let runH m = do
            (r,(muconf,_)) <- runStateT m (conf, mempty)
            writeIORef gstate muconf
            return r
        runActionToIO m = do
            muconf <- readIORef gstate
            evalStateT m (muconf, mempty)
    return (runH, runActionToIO)

scottyH :: Port -> ScottyH () -> IO ()
scottyH p s = do
    (runH, runActionToIO) <- mkHStateRunners defaultConfig
    scottyT p runH runActionToIO s

scottyHOpts :: Options -> ScottyH () -> IO ()
scottyHOpts opts s = do
    (runH, runActionToIO) <- mkHStateRunners defaultConfig
    scottyOptsT opts runH runActionToIO s

scottyHApp :: MuConfig IO -> ScottyH () -> IO Application
scottyHApp conf defs = do
    (runH, runActionToIO) <- mkHStateRunners conf
    scottyAppT runH runActionToIO defs

setTemplatesDir :: FilePath -> ScottyH ()
setTemplatesDir dir = do
  (conf :: MuConfig IO, tmap) <- lift State.get
  lift . State.put $ (conf { muTemplateFileDir = Just dir }, tmap)

setHastacheConfig :: MuConfig IO -> ScottyH ()
setHastacheConfig conf = do
  (_, tmap) <- lift State.get
  lift . State.put $ (conf, tmap)

hastache :: FilePath -> ActionT HState ()
hastache tpl = do
  ((conf :: MuConfig IO), tmap) <- lift State.get
  header "Content-Type" "text/html"
  let cntx a  = fromMaybe MuNothing (M.lookup a tmap)
  let tplFile = fromMaybe "." (muTemplateFileDir conf)
              </> tpl
              ++ fromMaybe "" (muTemplateFileExt conf)
  res <- liftIO $ hastacheFile conf tplFile (mkStrContext cntx)
  raw res

setH :: String -> MuType IO -> ActionT HState ()
setH x y = do
  (conf, tmap) <- lift State.get
  lift . State.put $ (conf, M.insert x y tmap)

instance Show Markup where
  show = BRS.renderHtml

instance MuVar Markup where
  isEmpty = isEmpty . BRU.renderHtml
  toLByteString = BRU.renderHtml


