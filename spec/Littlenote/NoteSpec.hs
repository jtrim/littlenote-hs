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

    describe "parseLines" $ do
      it "parses lines of notes into a list of Notes" $ do
        let lines = "01/01/1969 10:00:00 PM - wibble\n" ++
                    "01/01/1970 11:00:00 PM - fwaff\n" ++
                    "01/02/1971 12:00:00 AM - nuun"
            expectedNotes = [
              Subject.Note {
                Subject.recordedAt = Date.parse "01/01/1969 10:00:00 PM",
                Subject.text = "wibble"
              },
              Subject.Note {
                Subject.recordedAt = Date.parse "01/01/1970 11:00:00 PM",
                Subject.text = "fwaff"
              },
              Subject.Note {
                Subject.recordedAt = Date.parse "01/02/1971 12:00:00 AM",
                Subject.text = "nuun"
              }]

        Subject.parseLines lines `shouldBe` expectedNotes

    describe "renderLines" $ do
      it "renders [Note] into a littlenotes string" $ do
        let notes = [
              Subject.Note {
                Subject.recordedAt = Date.parse "01/01/1969 10:00:00 PM",
                Subject.text = "wibble"
              },
              Subject.Note {
                Subject.recordedAt = Date.parse "01/01/1970 11:00:00 PM",
                Subject.text = "fwaff"
              },
              Subject.Note {
                Subject.recordedAt = Date.parse "01/02/1971 12:00:00 AM",
                Subject.text = "nuun"
              }]
            expectedLines = "01/01/1969 10:00:00 PM - wibble\n" ++
                            "01/01/1970 11:00:00 PM - fwaff\n" ++
                            "01/02/1971 12:00:00 AM - nuun"

        Subject.renderLines notes `shouldBe` expectedLines

