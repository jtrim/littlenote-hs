module NotesFile (path, contents, lastNote, writeTempNotesFile, commitTempNotesFile, noteLines) where
  import System.IO.Temp   (openTempFile)
  import System.IO        (hPutStr, hClose)
  import System.Directory (renameFile, removeFile)

  path = "./littlenotes.txt"

  contents = do
    notes <- readFile path
    return notes

  noteLines = do
    c <- contents
    return $ lines c

  lastNote = do
    notes <- contents
    return $ last $ lines notes

  writeTempNotesFile contents = do
    (path, handle) <- openTempFile "/tmp" "littlenote-amend-result.txt"
    hPutStr handle contents
    hClose handle
    return path

  commitTempNotesFile tempFilePath = do
    renameFile tempFilePath path
