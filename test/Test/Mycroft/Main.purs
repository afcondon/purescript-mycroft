module Test.Mycroft.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Mycroft.IntegrationSpec as IntegrationSpec
import Test.Mycroft.SExprSpec as SExprSpec

main :: Effect Unit
main = launchAff_ do
  liftEffect SExprSpec.run
  IntegrationSpec.run
  liftEffect (log "All mycroft tests passed")
