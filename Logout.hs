{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Logout where

import Settings
import Foundation
import Yesod.Core
import qualified Data.Text as T

getLogoutR :: Handler Html
getLogoutR = do
    deleteCookie (T.pack userInfoKey) "/"
    redirect homePage