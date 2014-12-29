module Littlenote.ContextSpec where
  import Test.Hspec
  import Littlenote.Context
  import Data.Maybe (fromJust)

  spec = do
    describe "withNotePath" $ do
      it "sets a note path" $ do
        let getNotePath Context { notePath = np } = fromJust np
        let context = withNotePath "./fakenotes.txt" defaultContext
        getNotePath context `shouldBe` "./fakenotes.txt"
