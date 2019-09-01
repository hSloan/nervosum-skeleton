{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

import Prelude hiding (id, (.))
import Common.App
import Common.Route
import Control.Category
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.GADT.Show.TH
import qualified Data.Map.Monoidal as MMap
import Data.Pool
import Data.Semigroup
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration
import Database.Groundhog.Postgresql
import Obelisk.Backend
import Rhyolite.Account
import Rhyolite.Api
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Gargoyle
import Rhyolite.Backend.Listen
import Rhyolite.Backend.Logging
import Rhyolite.Backend.Sign
import Rhyolite.Schema
import qualified Web.ClientSession as CS

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      csk <- CS.getKey "clientSessionKey"
      withLogging [LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr $ Nothing) (Just mempty)] $ do
        logger <- fmap LoggingEnv askLoggerIO
        liftIO $ withDb "db" $ \db -> do
          _ <- runLoggingEnv logger $ do
            runDb (Identity db) $ do
              tableInfo <- getTableAnalysis
              addDefaultAccount
              runMigration $ do
                migrateAccount tableInfo
                -- migrateSchema tableInfo -- TODO: make sure migrations are handled for additional schemas

          (listen, _) <- serveDbOverWebsockets db
            (handleRequests logger csk db)
            (notifyHandler logger csk db)
            (viewSelectorHandler csk logger db)
            (queryMorphismPipeline $ transposeMonoidMap . monoidMapQueryMorphism)
          liftIO $ serve $ \case
            BackendRoute_Missing :=> _ -> listen -- TODO: create a 404 page not found route || THIS WILL CAUSE A PROBLEM
            BackendRoute_Listen :=> Identity () -> listen

  , _backend_routeEncoder = backendRouteEncoder
  }

-- TODO: As this function grows it is recommended that you create a module dedicated to handling request. 
handleRequests
  :: ( MonadIO m
     , MonadBaseNoPureAborts IO m
     )
  =>  LoggingEnv -> CS.Key -> Pool Postgresql -> RequestHandler DefApp m
handleRequests logger csk db = RequestHandler $ \req -> runLoggingEnv logger $ runDb (Identity db) $ case req of
  ApiRequest_Public r -> case r of
    PublicRequest_Login email pw -> do
      res <- login (signWithKey csk . AuthToken . Identity) email pw
      case res of
        Nothing -> return $ Left $ "Login failed"
        Just token -> return $ Right token
  ApiRequest_Private token r
    | Just (AuthToken (Identity _accId)) <- readSignedWithKey csk token -> case r of
        PrivateRequest_Bar _ -> return $ Right ()
    | otherwise -> error "Unable to authenticate private request"

-- TODO: Notifications matter when a change happens within a database and you need to let the frontend know to re-query
-- the database in order to update the view. It is recommeded that a module is created for notification handling.
data Notification :: * -> * where
  Notification_Foo :: Notification (Id Account)

-- Notifies the frontend when some data in the database has changed so that it may update the current view
notifyHandler :: forall a. Semigroup a => LoggingEnv -> CS.Key -> Pool Postgresql -> DbNotification Notification -> DefAppViewSelector a -> IO (DefAppView a)
notifyHandler _ _ _ = return mempty

-- TODO: As this function grows, it is recommeded that a module is created for handling view selectors  
-- Queries database for requested data and returns it to the frontend
viewSelectorHandler
  :: CS.Key
  -> LoggingEnv
  -> Pool Postgresql
  -> QueryHandler (DefAppViewSelector a) IO
viewSelectorHandler _csk logger db = QueryHandler $ \vs -> runLoggingEnv logger $ runDb (Identity db) $
  return DefAppView
    { _appDefView_echo = MMap.mapWithKey (\_ v -> (v, First (Just ""))) $ _appDefViewSelector_echo vs
    }

addDefaultAccount :: (PersistBackend m, SqlDb (PhantomDb m), MonadIO m) => m ()
addDefaultAccount = do
  (_,acc) <- ensureAccountExists Notification_Foo "foo@bar.com"
  setAccountPassword acc "asdf"

deriveJSONGADT ''Notification
deriveArgDict ''Notification
deriveGShow ''Notification

