{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeHoles #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | Hastache templating for Scotty
module Web.Scotty.Hastache where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Morph
import Blaze.ByteString.Builder (fromByteString)
import Web.Scotty as S
import Web.Scotty.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port, runSettings, settingsPort)
import Network.HTTP.Types
import Data.Default
import Control.Monad.State as State
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Posix
import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.Utf8 as BRU
import Text.Blaze.Html.Renderer.String as BRS

-- | State with Hastache config
type HState = StateT ((MuConfig IO, Map String (MuType IO))) IO 
                
type ScottyH = ScottyT HState
type ActionH = ActionT HState

scottyH :: Port -> ScottyH () -> IO ()
scottyH p = scottyHOpts $ def { settings = (settings def) { settingsPort = p } }

scottyHOpts :: Options -> ScottyH () -> IO ()
scottyHOpts opts s = do
  when (verbose opts > 0) $
    putStrLn $ "Setting phasers to stun... (port " ++ show (settingsPort (settings opts)) ++ ") (ctrl-c to quit)"
  runSettings (settings opts) =<< scottyHApp s defaultConfig

  
scottyHApp :: ScottyH () -> MuConfig IO -> IO Application
scottyHApp defs conf = do
  (s, (muconf,_)) <- runStateT (execStateT (runScottyT defs) def) (conf, mempty)
  routes' <- mapM (hroute muconf) (routes s)
  return $ foldl (flip ($)) notFoundApp $  routes' ++ middlewares s

hroute :: MuConfig IO -- ^ The initial config
       -> MiddlewareT HState
       -> IO Middleware
hroute conf mw = return $ \app -> 
  let (r :: ApplicationT HState) = mw app
  --- ApplicationT HState == Request -> ResourceT HState Response
  in hoist (flip evalStateT (conf, mempty)) . r

notFoundApp :: Application
notFoundApp _ = return $ ResponseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"


setTemplatesDir :: FilePath -> ScottyH ()
setTemplatesDir dir = do
  (conf :: MuConfig IO, map) <- lift State.get
  lift . State.put $ (conf { muTemplateFileDir = Just dir }, map)

setHastacheConfig :: MuConfig IO -> ScottyH ()
setHastacheConfig conf = do
  (_, map) <- lift State.get
  lift . State.put $ (conf, map)

hastache :: FilePath -> ActionT HState ()
hastache tpl = do
  ((conf :: MuConfig IO), map) <- lift State.get
  header "Content-Type" "text/html"
  let cntx a  = fromMaybe MuNothing (M.lookup a map)
  let tplFile = fromMaybe "." (muTemplateFileDir conf)
              </> tpl
              ++ fromMaybe "" (muTemplateFileExt conf)
  res <- liftIO $ hastacheFile conf tplFile (mkStrContext cntx)
  raw res

setH :: String -> MuType IO -> ActionT HState ()
setH x y = do
  (conf, map) <- lift State.get
  lift . State.put $ (conf, M.insert x y map)

instance Show Markup where
  show = BRS.renderHtml

instance MuVar Markup where
  isEmpty = isEmpty . BRU.renderHtml
  toLByteString = BRU.renderHtml


