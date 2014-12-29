module Littlenote.Persistence.FlatFile (lines, saveNotes) where
  import Littlenote.Note (parse, Note(..))
  import Littlenote.Context (Context(..))
  import qualified Littlenote.Date as D (format)
  import System.IO (readFile, hPutStr, hClose, openTempFile)
  import System.Directory (getTemporaryDirectory, renameFile)
  import Prelude hiding (lines)
  import qualified Data.List as L (lines, intercalate)

  lines Context { notePath = Just notePath } = do
    contents <- readFile notePath
    return $ map parse $ L.lines contents

  saveNotes :: Context -> [Note] -> IO ()
  saveNotes (Context { notePath = Just np }) notes = do
    withNotesFile np $ \handle ->
      hPutStr handle $ renderNotes notes

  renderNotes notes = L.intercalate "\n" $ map renderNote notes

  renderNote (Note {
    text = text,
    recordedAt = recordedAt
  }) = D.format recordedAt ++ " - " ++ text

  withNotesFile path fn = do
    tempDir <- getTemporaryDirectory
    (tmppath, handle) <- openTempFile tempDir "littlenotes.txt"
    fn handle
    hClose handle
    renameFile tmppath path
