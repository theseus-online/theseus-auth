{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    makeSessionBackend _ = return Nothing       -- We don't use session
