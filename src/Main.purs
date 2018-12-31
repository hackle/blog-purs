module Main where

import Prelude
import Effect (Effect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))
import Data.Maybe (Maybe (..))
import Halogen as H

import Component as C

main :: Effect Unit
main = HA.runHalogenAff do
  el <- HA.selectElement (QuerySelector "#main")
  case el of
    Nothing -> throwError $ error "no container  found"
    Just container -> do
      io <- runUI C.component unit container
      io.query $ H.action C.Init
