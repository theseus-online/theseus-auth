{-# LANGUAGE ImplicitParams #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Settings
import Options.Applicative
import Data.Semigroup ((<>))

configPath :: Parser FilePath
configPath = strOption
           ( long "config"
          <> short 'c'
          <> metavar "PATH"
          <> help "Path to config file"
           )

main :: IO ()
main = do
    execParser opts >>= parseConfig
    warp 8090 App
    where
        opts = info (configPath <**> helper)
             ( fullDesc
            <> progDesc "Auth user and set encrypted cookie"
            <> header "theseus-auth - auth module for theseus.online"
             )
