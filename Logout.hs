{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Logout where

import Settings
import Foundation
import Yesod.Core
import qualified Data.Text as T

getLogoutR :: Handler Html
getLogoutR = do
    key <- liftIO (theseusConfig >>= \c -> return $ theseusUserInfoKey c)
    home <- liftIO (theseusConfig >>= \c -> return $ theseusHomePage c)
    deleteCookie (T.pack key) "/"
    redirect home
