module Actions (determineAction, printNotes) where
  import Data.List (find, intercalate)
  import Arguments (name)
  import Options   (Options(..), defaultOptions)
  import NotesFile (path, lastNote, contents, writeTempNotesFile, commitTempNotesFile)
  import System.Posix.Process (getProcessStatus, forkProcess, executeFile)
  import System.IO (hPutStr, hClose, readFile, withFile, IOMode(WriteMode))
  import System.IO.Temp (openTempFile)

  determineAction [] = printNotes
  determineAction (arg:args) = case foundAction of
    Nothing -> determineAction args
    Just pair -> snd pair
    where
      isTargetAction targetName currentAction = targetName == fst currentAction
      foundAction = find (isTargetAction $ name arg) actions

  actions = [
      ("-h", printUsage),
      ("--help", printUsage),
      ("-e", editNotes),
      ("--edit", editNotes),
      ("--amend", amendLastNote)
    ]

  printUsage options = do
    putStrLn "TODO"

  printNotes (Options { printBack = printBackLinesCount }) = do
    notes <- readFile NotesFile.path
    putStr $ if printBackLinesCount < 0
    then
      notes
    else
      unlines $ reverse $ take printBackLinesCount $ reverse $ lines notes

  editNotes _ = do
    pid <- forkProcess $ openVim NotesFile.path
    _ <- waitFor pid
    printNotes defaultOptions

  openVim filePath = do
    executeFile "vim" True [filePath] Nothing

  waitFor pid = do
    getProcessStatus True False pid

  amendLastNote options = do
    (path, handle) <- openTempFile "/tmp" "littlenote-amend.txt"
    lastNote <- NotesFile.lastNote
    hPutStr handle lastNote
    hClose handle

    pid <- forkProcess $ openVim path
    _ <- waitFor pid

    amendedNote <- readFile path
    allNotes <- NotesFile.contents

    tempNotesPath <- writeTempNotesFile $ replaceLastNote allNotes amendedNote
    commitTempNotesFile tempNotesPath

    printNotes defaultOptions

  replaceLastNote notes updatedNote = intercalate "\n" $ (init $ lines notes) ++ [updatedNote]
