{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    , api
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Models.Edition
import Models.Fragment

type API =
       "editions" :> Get '[JSON] [Edition]
  :<|> "fragments" :> Get '[JSON] [Fragment]

startApp :: IO ()
startApp = do
  putStrLn "Igniting port 8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
         return editions
    :<|> return fragments
