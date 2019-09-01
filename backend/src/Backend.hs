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
import Backend.Notification
import Backend.ViewSelector
import Common.App
import Common.Route
import Control.Category
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Pool
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
import Rhyolite.Backend.Logging
import Rhyolite.Backend.Sign
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
            BackendRoute_Missing :=> _ -> listen -- TODO: create a 404 page not found route
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

addDefaultAccount :: (PersistBackend m, SqlDb (PhantomDb m), MonadIO m) => m ()
addDefaultAccount = do
  (_,acc) <- ensureAccountExists Notification_Foo "foo@bar.com"
  setAccountPassword acc "asdf"

