{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Frontend.App where 

import Rhyolite.Frontend.App
import Common.App

type AppWidget t m = MonadRhyoliteFrontendWidget DefApp t m
