-- File auto generated by servant-purescript! --
module API.MakeRequests where

import Prelude

import API (SPParams_(..))
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable(), toNullable)
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent)
import Models.Edition (Edition)
import Models.Fragment (Fragment)
import Network.HTTP.Affjax (AJAX)
import Prim (Array, String)
import Servant.PureScript.Affjax (AjaxError(..), affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettings_(..), gDefaultToURLPiece)
import Servant.PureScript.Util (encodeHeader, encodeListQuery, encodeQueryItem, encodeURLPiece, getResult)
import Servant.Subscriber (ToUserType)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Subscriptions (Subscriptions, makeSubscriptions)
import Servant.Subscriber.Types (Path(..))
import Servant.Subscriber.Util (TypedToUser, subGenFlagQuery, subGenListQuery, subGenNormalQuery, toUserType)

getEditions :: forall m. MonadReader (SPSettings_ SPParams_) m => m HttpRequest
getEditions = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["editions"]
  let reqHeaders =
        []
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

getFragments :: forall m. MonadReader (SPSettings_ SPParams_) m => m HttpRequest
getFragments = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["fragments"]
  let reqHeaders =
        []
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

