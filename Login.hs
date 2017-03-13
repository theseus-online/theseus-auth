{-# LANGUAGE OverloadedStrings #-}
module Login where

import Settings
import Foundation
import Data.Time (secondsToDiffTime)
import Data.Aeson (object, (.=), encode)
import Data.Aeson.Lens (_String, key)
import Control.Monad.Trans (liftIO)
import Control.Lens ((.~), (^.), (&))
import Network.Wreq (param, defaults, responseBody, getWith, header)
import Web.Cookie (setCookieName, setCookiePath, setCookieValue, setCookieMaxAge, def)
import qualified Yesod.Core as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T

getLoginR :: Handler C.Html
getLoginR = do
    urlMaybe <- C.lookupGetParam "back_url"
    case urlMaybe of
        Just url -> C.redirect $ T.pack (buildUrl githubAuthUrl [("client_id", clientID), ("redirect_uri", backUrl), ("state", T.unpack url)]) 
        Nothing -> C.redirect $ T.pack (buildUrl githubAuthUrl [("client_id", clientID), ("redirect_uri", backUrl), ("state", homePage)])
    where 
        buildUrl :: String -> [(String, String)] -> String
        buildUrl uri ps = uri ++ "?" ++ buildParams ps

        buildParams :: [(String, String)] -> String
        buildParams ps = tail $ foldr (\(x, y) z -> z ++ "&" ++ x ++ "=" ++ y) "" ps

getGithubCallbackR :: Handler C.Html
getGithubCallbackR = do
    codeMaybe <- C.lookupGetParam "code"
    stateMaybe <- C.lookupGetParam "state"
    case codeMaybe of
        Just code -> do
            token <- processCode (T.unpack code)
            userInfo <- processToken token
            setUserInfo userInfo
            C.redirect $ case stateMaybe of
                Just state -> state
                Nothing -> T.pack homePage
        Nothing -> C.redirect LoginR
    
    where
        processCode :: String -> C.HandlerT App IO String
        processCode code = do
            let opts = defaults 
                                    & param "code" .~ [T.pack code] 
                                    & param "client_id" .~ [T.pack clientID] 
                                    & param "client_secret" .~ [T.pack clientSecret] 
                                    & param "redirect_uri" .~ [T.pack backUrl]
                                    & header "Accept" .~ ["application/json"]
            resp <- liftIO $ getWith opts githubTokenUrl
            case (resp ^. responseBody . key "access_token" . _String) of
                "" -> return ""
                token -> return (T.unpack token)

        processToken :: String -> C.HandlerT App IO String
        processToken token = do
            let opts = defaults & param "access_token" .~ [T.pack token]
            resp <- liftIO $ getWith opts githubUserUrl
            return $ LB.unpack $ encode $ object [
                                                    "username" .= (resp ^. responseBody . key "login" . _String), 
                                                    "avatar" .= (resp ^. responseBody . key "avatar_url" . _String),
                                                    "email" .= (resp ^. responseBody . key "email" . _String)]

        setUserInfo :: String -> C.HandlerT App IO ()
        setUserInfo info = do
            let c = def { 
                setCookieName = B.pack userInfoKey,
                setCookieValue = B.pack info,
                setCookiePath = Just "/",
                setCookieMaxAge = Just $ secondsToDiffTime 604800           -- 7 days
            }
            C.setCookie c
