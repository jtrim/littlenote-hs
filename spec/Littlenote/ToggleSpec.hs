module Littlenote.ToggleSpec where
  import Test.Hspec
  import qualified Littlenote.Toggle as Subject

  spec = do
    describe "extractToggle" $ do
      let toggle = Subject.ValueToggle "-f" "--foo"

      it "correctly extracts a value argument from a short identifier" $ do
        Subject.extractToggle ("-f", "bar") [toggle] `shouldBe` Just (toggle, Just "bar")

      it "correctly extracts a value argument from a long identifier" $ do
        Subject.extractToggle ("--foo", "bar") [toggle] `shouldBe` Just (toggle, Just "bar")

      it "supplies Nothing when no known toggle matches" $ do
        Subject.extractToggle ("--nothing", "") [] `shouldBe` Nothing

    describe "toggles" $ do
      let toggleA = Subject.ValueToggle "-f" "--foo"
          toggleB = Subject.BooleanToggle "-h" "--help"
          toggles = [toggleA, toggleB]

      it "extracts a series of toggles from an argument list" $ do
        Subject.toggles ["-f", "10", "--help", "--doesnt-exist"] toggles
          `shouldBe` [(toggleA, Just "10"), (toggleB, Nothing)]

      it "ignores anything past the occurence of -- in the argument list" $ do
        let toggles = [toggleA, toggleB, Subject.BooleanToggle "-w" "--wat"]
        Subject.toggles ["-f", "10", "--help", "--doesnt-exist", "--", "--wat"] toggles
          `shouldBe` [(toggleA, Just "10"), (toggleB, Nothing)]
