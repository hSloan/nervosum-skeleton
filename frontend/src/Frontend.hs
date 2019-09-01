{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Frontend where

import Common.Route
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom
import qualified Obelisk.ExecutableConfig as Cfg
import Common.App
import Rhyolite.Api
import Rhyolite.Frontend.App

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "User App Template"
  , _frontend_body = do
    configRoute <- liftIO $ Cfg.get "config/common/route"
    let websocketUrl = case configRoute of
          Nothing -> error "Invalid websocket route"
          Just wsroute -> toWebSocketAddr $ wsroute <> (renderBackendRoute checkedRouteEncoder $ BackendRoute_Listen :/ ())
    _ <- prerender blank $ void $ runRhyoliteWidget @DefApp websocketUrl entryPage
    pure ()
  }
  where
    -- TODO: There needs to be a text manipulating function similar to websocketUri from Rhyolite
    toWebSocketAddr address = case T.unpack address of
      'h':'t':'t':'p':'s':uri -> T.pack $ 'w':'s':'s':uri
      'h':'t':'t':'p':uri -> T.pack $ 'w':'s':uri
      _ -> error "Invalid websocket route"

entryPage :: MonadRhyoliteFrontendWidget DefApp t m => m ()
entryPage = do
  email <- inputElement def
  password <- inputElement def
  login <- button "Login"
  -- TODO: Create and send request to login. 
  let requestPrep = (\e p -> ApiRequest_Public $ PublicRequest_Login e p)
        <$> value email
        <*> value password
      loginReq = tagPromptlyDyn requestPrep login
  loginResp <- requestingIdentity loginReq
  widgetHold_ blank $ ffor loginResp $ \case
    Left _ -> text "Login Failed"
    Right _ -> text "Login Success"
  -- TODO: receive token and create cookie with token
  -- TODO: when login successful, redirect to a login page (use actual route or widgets?)
  return ()
