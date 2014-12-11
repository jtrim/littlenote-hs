module Actions (determineAction, printNotes) where
  import Data.List (find)
  import Arguments (name)
  import Options   (Options(..), defaultOptions)
  import NotesFile (path)
  import System.Posix.Process (getProcessStatus, forkProcess, executeFile)

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
      ("--edit", editNotes)
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
