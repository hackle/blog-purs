module Component where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), fromRight)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Affjax as AX
import Affjax.ResponseFormat (json, ResponseFormat) as RF
import Effect.Aff (Aff)
import Affjax.RequestHeader
import Data.MediaType
import Foreign.Object
import Data.Argonaut.Core

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
    
    req :: AX.Request Json
    req = AX.defaultRequest {
                            url = "https://iit8qnfbeb.execute-api.ap-southeast-2.amazonaws.com/prod"
                            , responseFormat = RF.json
                            , headers = [ Accept (MediaType "text/markdown") ]
                            }

    eval :: Query ~> H.ComponentDSL State Query Void Aff
    eval = case _ of
        Init next -> do
            response <- H.liftAff $ AX.request req
            case response.body of
                Left _ -> pure next
                Right resp -> do
                    _ <- H.modify (\state -> decodeState resp)
                    pure next

    decodeState :: Json -> State
    decodeState js = caseJsonObject initialState (\o -> fromMaybe initialState $ toState o) js
        where
            toState :: Object Json -> Maybe State
            toState obj = do
                t <- lookup "title" obj
                c <- lookup "content" obj
                Just { title: caseJsonString "-" (\a -> a) t, content: caseJsonString "-" (\a -> a) c }