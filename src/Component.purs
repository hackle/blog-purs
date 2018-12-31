module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), fromRight)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Affjax as AX
import Affjax.ResponseFormat (string, ResponseFormat) as RF
import Effect.Aff (Aff)
import Affjax.RequestHeader
import Data.MediaType

data Query a = Init a

type State = { title :: String, content :: String }

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { title: "Loading", content: "Loading" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text state.title ]
      , HH.div_
          [ HH.text state.content ]
      ]
  
  req :: AX.Request String
  req = AX.defaultRequest {
                        url = "https://iit8qnfbeb.execute-api.ap-southeast-2.amazonaws.com/prod"
                        , responseFormat = RF.string
                        , headers = [ Accept (MediaType "text/markdown") ]
                        }

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    Init next -> do
        response <- H.liftAff $ AX.request req
        case response.body of
            Left _ -> pure next
            Right resp -> do
                _ <- H.modify (\state -> { title: "loaded", content: resp })
                pure next