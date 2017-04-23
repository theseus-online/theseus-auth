{-# LANGUAGE OverloadedStrings    #-}

module Settings (Config(..), theseusConfig, parseConfig) where

import Control.Concurrent.MVar (MVar, putMVar, readMVar, newEmptyMVar)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)

data Config = Config
            { githubClientID :: String
            , githubClientSecret :: String
            , githubBackUrl :: String
            , githubAuthUrl :: String
            , githubTokenUrl :: String
            , githubUserUrl :: String
            , theseusUserInfoKey :: String
            , theseusDBInterface :: String
            , theseusHomePage :: String
            , theseusDomain :: String
            } deriving (Show)

instance FromJSON Config where
    parseJSON (Y.Object o) = Config
                         <$> o .: "github-client-id"
                         <*> o .: "github-client-secret"
                         <*> o .:? "github-back-url" .!= "http://theseus.online/auth/github-callback"
                         <*> o .:? "github-auth-url" .!= "https://github.com/login/oauth/authorize"
                         <*> o .:? "github-token-url" .!= "https://github.com/login/oauth/access_token"
                         <*> o .:? "github-user-url" .!= "https://api.github.com/user"
                         <*> o .:? "theseus-userinfo-key" .!= "theseus-userinfo"
                         <*> o .:? "theseus-db-interface" .!= "http://db.theseus-online"
                         <*> o .:? "theseus-home-page" .!= "http://theseus.online"
                         <*> o .:? "theseus-domain" .!= "theseus.online"
    parseJSON _ = error "parse config failed: expect object"

globalConfig :: MVar Config
globalConfig = unsafePerformIO newEmptyMVar

parseConfig :: FilePath -> IO ()
parseConfig fpath = do
    fcontent <- B.readFile fpath
    case Y.decode fcontent of
        Just cfg -> putMVar globalConfig cfg
        Nothing -> error "parse config failed"

theseusConfig :: IO Config
theseusConfig = readMVar globalConfig
