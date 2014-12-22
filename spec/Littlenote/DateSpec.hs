module Littlenote.DateSpec where
  import Test.Hspec
  import Data.Time.Format (parseTimeM, defaultTimeLocale)
  import Data.Time.LocalTime (LocalTime)
  import Data.Maybe (fromJust)
  import qualified Littlenote.Date as Subject

  specContext = let
    input  = "11/05/1968 10:23:48 AM"
    parser = parseTimeM True defaultTimeLocale "%m/%d/%Y %I:%M:%S %p"
    mtime  = parser input :: Maybe LocalTime
    time   = fromJust mtime
    in
      (input, time)

  spec = do
    describe "format" $ do
      it "formats a date properly" $ do
        let (input, time) = specContext
        Subject.format time `shouldBe` input

    describe "parse" $ do
      it "parses a correctly formatted date" $ do
        let (input, time) = specContext
        Subject.parse input `shouldBe` time

