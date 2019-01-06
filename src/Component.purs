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
import Text.Markdown.SlamDown.Halogen.Component
import Text.Markdown.SlamDown.Parser
import Data.Foldable (for_)
import Prism

data Query a = Init a
                -- | ChangeDocument String a

type State = { title :: String, content :: String }

data SlamDownSlot = SlamDownSlot
derive instance slamDownSlotEq :: Eq SlamDownSlot
derive instance slamDownSlotOrd :: Ord SlamDownSlot

component :: H.Component HH.HTML Query Unit Void Aff
component =
    H.parentComponent
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where

    initialState :: State
    initialState = { title: "Loading", content: "Loading" }

    render :: State -> H.ParentHTML Query (SlamDownQuery String) SlamDownSlot Aff
    render state =
        HH.div_
        [ HH.h1_
            [ HH.text state.title ]
        , HH.div_
            [ HH.slot SlamDownSlot slamDownComponent unit absurd ]
        ]
    
    req :: AX.Request Json
    req = AX.defaultRequest {
                            url = "https://iit8qnfbeb.execute-api.ap-southeast-2.amazonaws.com/prod"
                            , responseFormat = RF.json
                            , headers = [ Accept (MediaType "application/json") ]
                            }

    eval :: Query ~> H.ParentDSL State Query (SlamDownQuery String) SlamDownSlot Void Aff
    eval q = case q of
        Init next -> do
            response <- H.liftAff $ AX.request req
            case response.body of
                Left _ -> pure next
                Right raw -> do
                    let resp = decodeState raw
                    _ <- H.modify (\state -> resp)
                    for_ (parseMd resp.content) \md -> H.query SlamDownSlot (H.action (SetDocument md))
                    _ <- H.liftEffect $ rerenderMd unit
                    pure next

    decodeState :: Json -> State
    decodeState js = caseJsonObject initialState (\o -> fromMaybe initialState $ toState o) js
        where
            toState :: Object Json -> Maybe State
            toState obj = do
                t <- lookup "title" obj
                c <- lookup "content" obj
                Just { title: caseJsonString "-" (\a -> a) t, content: caseJsonString "-" (\a -> a) c }