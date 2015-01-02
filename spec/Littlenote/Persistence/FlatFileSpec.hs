module Littlenote.Persistence.FlatFileSpec where
  import Test.Hspec
  import qualified Littlenote.Persistence.FlatFile as Subject
  import qualified Littlenote.Context as C
  import qualified Littlenote.Note as N
  import qualified System.IO as IO (readFile)

  spec = do
    context "within an application context" $ do
      describe "getNotes" $ do
        it "reads note lines from a file" $ do
          let applicationContext = C.withNotePath "./spec/fixtures/notes.txt" C.defaultContext
          lines' <- Subject.getNotes applicationContext
          lines' `shouldBe` [
              N.parse "12/09/14 10:13:20 AM - Fixture note 1",
              N.parse "12/09/14 10:13:33 AM - Fixture note 2"
            ]

      describe "saveNotes" $ do
        let applicationContext = C.withNotePath "./tmp/littlenotes-spec.txt" C.defaultContext
            saveNotes = Subject.saveNotes applicationContext [
              N.parse "12/09/14 10:13:20 AM - Fixture note 1",
              N.parse "12/09/14 10:13:33 AM - Fixture note 2"]

        it "saves Notes to a file" $ do
          saveNotes
          newContents <- IO.readFile "./tmp/littlenotes-spec.txt"
          newContents `shouldBe`
            "12/09/14 10:13:20 AM - Fixture note 1\n" ++
            "12/09/14 10:13:33 AM - Fixture note 2"



