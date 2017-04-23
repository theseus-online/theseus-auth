{-# LANGUAGE OverloadedStrings #-}
module Login where

import Settings
import Foundation
import Data.Time (secondsToDiffTime)
import Data.Aeson (object, (.=), encode)
import Data.Aeson.Lens (_String, key)
import Control.Monad.Trans (liftIO)
import Control.Lens ((.~), (^.), (&))
import Network.Wreq (FormParam((:=)), param, defaults, responseBody, getWith, header, post)
import Web.Cookie (setCookieName, setCookiePath, setCookieValue, setCookieMaxAge, def)
import qualified Yesod.Core as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T

getLoginR :: Handler C.Html
getLoginR = do
    backUrl <- liftIO (theseusConfig >>= \c -> return $ githubBackUrl c)
    homePage <- liftIO (theseusConfig >>= \c -> return $ theseusHomePage c)
    clientID <- liftIO (theseusConfig >>= \c -> return $ githubClientID c)
    authUrl <- liftIO (theseusConfig >>= \c -> return $ githubAuthUrl c)
    urlMaybe <- C.lookupGetParam "back_url"
    case urlMaybe of
        Just url -> C.redirect $ T.pack (buildUrl authUrl [("client_id", clientID), ("redirect_uri", backUrl), ("state", T.unpack url)])
        Nothing -> C.redirect $ T.pack (buildUrl authUrl [("client_id", clientID), ("redirect_uri", backUrl), ("state", homePage)])
    where
        buildUrl :: String -> [(String, String)] -> String
        buildUrl uri ps = uri ++ "?" ++ buildParams ps

        buildParams :: [(String, String)] -> String
        buildParams ps = tail $ foldr (\(x, y) z -> z ++ "&" ++ x ++ "=" ++ y) "" ps

getGithubCallbackR :: Handler C.Html
getGithubCallbackR = do
    homePage <- liftIO (theseusConfig >>= \c -> return $ theseusHomePage c)
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
            tokenUrl <- liftIO (theseusConfig >>= \c -> return $ githubTokenUrl c)
            backUrl <- liftIO (theseusConfig >>= \c -> return $ githubBackUrl c)
            clientID <- liftIO (theseusConfig >>= \c -> return $ githubClientID c)
            clientSecret <- liftIO (theseusConfig >>= \c -> return $ githubClientSecret c)
            let opts = defaults
                                    & param "code" .~ [T.pack code]
                                    & param "client_id" .~ [T.pack clientID]
                                    & param "client_secret" .~ [T.pack clientSecret]
                                    & param "redirect_uri" .~ [T.pack backUrl]
                                    & header "Accept" .~ ["application/json"]
            resp <- liftIO $ getWith opts tokenUrl
            case (resp ^. responseBody . key "access_token" . _String) of
                "" -> return ""
                token -> return (T.unpack token)

        processToken :: String -> C.HandlerT App IO String
        processToken token = do
            dbInterface <- liftIO (theseusConfig >>= \c -> return $ theseusDBInterface c)
            userUrl<- liftIO (theseusConfig >>= \c -> return $ githubUserUrl c)
            let opts = defaults & param "access_token" .~ [T.pack token]
            resp <- liftIO $ getWith opts userUrl
            let name = resp ^. responseBody . key "login" . _String
            let email = resp ^. responseBody . key "email" . _String
            let avatar = resp ^. responseBody . key "avatar_url" . _String
            _ <- liftIO $ post (dbInterface ++ "/rpc/create_or_update_user") [ "name" := name
                                                                             , "email" := email
                                                                             , "avatar" := avatar
                                                                             ]
            let infoStr = LB.unpack $ encode $ object [ "name" .= name
                                                      , "email" .= email
                                                      , "avatar" .= avatar
                                                      ]
            return infoStr

        setUserInfo :: String -> C.HandlerT App IO ()
        setUserInfo info = do
            userInfoKey <- liftIO (theseusConfig >>= \c -> return $ theseusUserInfoKey c)
            let c = def {
                setCookieName = B.pack userInfoKey,
                setCookieValue = B.pack info,
                setCookiePath = Just "/",
                setCookieMaxAge = Just $ secondsToDiffTime 604800           -- 7 days
            }
            C.setCookie c
