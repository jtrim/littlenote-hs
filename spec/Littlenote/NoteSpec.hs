module Littlenote.NoteSpec where
  import Test.Hspec
  import qualified Littlenote.Note as Subject
  import qualified Littlenote.Date as Date

  specContext = let
    noteTime = "11/05/1968 10:23:48 AM"
    noteText = "foo bar baz"
    note     = noteTime ++ " - " ++ noteText
    time     = Date.parse noteTime
    noteRecord = Subject.Note {
                   Subject.recordedAt = time,
                   Subject.text       = noteText
                 }
    in
      (note, time, noteText, noteRecord)

  spec = do
    describe "parse" $ do
      it "correctly parses a note" $ do
        let (note, time, noteText, expected) = specContext
        Subject.parse note `shouldBe` expected

    describe "render" $ do
      it "correctly renders a note" $ do
        let (note, _, _, noteRecord) = specContext
        Subject.render noteRecord `shouldBe` note
