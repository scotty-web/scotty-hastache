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

import           Control.Arrow                   ((***))
import           Control.Monad.State             as State
import           Data.IORef                      (newIORef, readIORef,
                                                  writeIORef)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (mempty)
import           Data.Text.Lazy                  (Text)
import           Network.Wai                     (Application, Response)
import           Network.Wai.Handler.Warp        (Port)
import           System.FilePath.Posix           ((</>))
import           Text.Hastache
import           Text.Hastache.Context

import           Web.Scotty.Trans                as S

-- * Runners and types

-- | The runner to use instead of 'scotty'
scottyH :: (ScottyError e) => Port -> ScottyH e () -> IO ()
scottyH p s = do
    (runH, runActionToIO) <- mkHStateRunners defaultConfig
    scottyT p runH runActionToIO s

-- | The runner to use instead of 'scottyOpts'
scottyHOpts :: (ScottyError e) => Options -> ScottyH e () -> IO ()
scottyHOpts opts s = do
    (runH, runActionToIO) <- mkHStateRunners defaultConfig
    scottyOptsT opts runH runActionToIO s

-- | A type synonym for @ScottyT e HState@; with custom exception types
type ScottyH e = ScottyT e HState

-- | A type synonym for @ScottyT e HState@; with custom exception types
type ActionH e = ActionT e HState

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
setHastacheConfig conf = do
  (_, tmap) <- lift State.get
  lift . State.put $ (conf, tmap)

-- | Modify the Hastache configuration as whole
modifyHastacheConfig :: (MuConfig IO -> MuConfig IO) -> ScottyH e ()
modifyHastacheConfig f = lift $ State.modify (f *** id)

-- | Set the path to the directory with templates. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplatesDir :: FilePath -> ScottyH e ()
setTemplatesDir dir = do
  lift $ State.modify $ \(conf :: MuConfig IO, tmap) ->
      (conf { muTemplateFileDir = Just dir }, tmap)

-- | Set the default extension for template files. This affects
-- how /both/ 'hastache' and the @{{> template}}@ bit searches for the
-- template files.
setTemplateFileExt :: String -> ScottyH e ()
setTemplateFileExt ext = do
  lift $ State.modify $ \(conf :: MuConfig IO, tmap) ->
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
hastache :: ScottyError e => FilePath -> ActionH e ()
hastache tpl = do
  ((conf :: MuConfig IO), tmap) <- lift State.get
  let cntx a  = fromMaybe MuNothing (M.lookup a tmap)
  let tplFile = fromMaybe "." (muTemplateFileDir conf)
              </> tpl
              ++ fromMaybe "" (muTemplateFileExt conf)
  res <- liftIO $ hastacheFile conf tplFile (mkStrContext cntx)
  html res

-- | Set the value of a mustache variable.
setH :: ScottyError e => String -> MuType IO -> ActionH e ()
setH x y = do
  (conf, tmap) <- lift State.get
  lift . State.put $ (conf, M.insert x y tmap)

-- * Internals

-- | State with the Hastache config
type HState = StateT (MuConfig IO, M.Map String (MuType IO)) IO

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

scottyHApp :: MuConfig IO -> ScottyH e () -> IO Application
scottyHApp conf defs = do
    (runH, runActionToIO) <- mkHStateRunners conf
    scottyAppT runH runActionToIO defs


