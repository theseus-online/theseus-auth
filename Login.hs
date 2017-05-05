{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Login where

import Settings
import Foundation
import Data.Time (secondsToDiffTime)
import Data.Aeson (object, (.=), encode)
import Data.Aeson.Lens (_String, key)
import Control.Monad.Trans (liftIO)
import Control.Lens ((.~), (^.), (&))
import Network.Wreq (param, defaults, responseBody, getWith, header, put)
import Web.Cookie (setCookieName, setCookiePath, setCookieValue, setCookieMaxAge, def)
import qualified Yesod.Core as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Base16 as H

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
            api <- liftIO (theseusConfig >>= \c -> return $ theseusApiBackend c)
            userUrl<- liftIO (theseusConfig >>= \c -> return $ githubUserUrl c)
            let opts = defaults & param "access_token" .~ [T.pack token]
            resp <- liftIO $ getWith opts userUrl
            let name = resp ^. responseBody . key "login" . _String
            let email = resp ^. responseBody . key "email" . _String
            let avatar = resp ^. responseBody . key "avatar_url" . _String
            _ <- liftIO $ put (api ++ "/users/" ++ (T.unpack name)) $ object [ "name" .= name
                                                                             , "email" .= email
                                                                             , "avatar" .= avatar
                                                                             ]
            (timeStamp :: Integer) <- liftIO $ getPOSIXTime >>= (return . round)
            sigKey <- liftIO (theseusConfig >>= \c -> return $ signatureKey c)
            let signature = SHA1.finalize
                          $ SHA1.updates SHA1.init
                          $ map encodeUtf8 [name, email, avatar, (T.pack . show) timeStamp, T.pack sigKey]
            let infoStr = LB.unpack $ encode $ object [ "name" .= name
                                                      , "email" .= email
                                                      , "avatar" .= avatar
                                                      , "timestamp" .= (T.pack . show) timeStamp
                                                      , "signature" .= (B.unpack . H.encode) signature
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
