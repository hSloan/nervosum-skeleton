{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Notification where

import Common.App
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.GADT.Show.TH
import Data.Pool
import Database.Groundhog.Postgresql
import Rhyolite.Account
import Rhyolite.Backend.Listen
import Rhyolite.Backend.Logging
import Rhyolite.Schema
import qualified Web.ClientSession as CS

-- the database in order to update the view. It is recommeded that a module is created for notification handling.
data Notification :: * -> * where
  Notification_Foo :: Notification (Id Account)

-- Notifies the frontend when some data in the database has changed so that it may update the current view
notifyHandler :: forall a. Semigroup a => LoggingEnv -> CS.Key -> Pool Postgresql -> DbNotification Notification -> DefAppViewSelector a -> IO (DefAppView a)
notifyHandler _ _ _ = return mempty

deriveJSONGADT ''Notification
deriveArgDict ''Notification
deriveGShow ''Notification

