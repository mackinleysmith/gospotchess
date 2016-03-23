{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Api.Core
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App { _api :: Snaplet Api
               , _heist :: Snaplet (Heist App) }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App
