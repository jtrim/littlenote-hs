module Main where
  import System.Environment  (getArgs)
  import Data.Time.LocalTime (getZonedTime)
  import Data.Time.Format    (formatTime, defaultTimeLocale)
  import Data.List           (intercalate)
  import Options             (Options(..), defaultOptions, applyOptions)
  import Arguments           (determineArguments)
  import Actions             (determineAction, printNotes)
  import NotesFile           (path)

  main = do
    args <- getArgs
    processInput args

  processInput [] = printNotes defaultOptions
  processInput rawArgs =
    let
      arguments    = determineArguments rawArgs
      options      = applyOptions arguments
      action       = determineAction arguments
      incomingNote = getIncomingNote rawArgs
      in
        if incomingNote /= ""
        then
          do
            newNote <- formatNoteLine $ getIncomingNote rawArgs
            appendFile NotesFile.path newNote
        else
          action options

  currentFormattedTime = do
    t <- getZonedTime
    return (formatTime defaultTimeLocale "%D %r" t)

  formatNoteLine s = do
    formattedTime <- currentFormattedTime
    return $ formattedTime ++ " - " ++ s ++ "\n"

  getIncomingNote []   = ""
  getIncomingNote ("--":postDashyArgs) = intercalate " " postDashyArgs
  getIncomingNote args = getIncomingNote $ dropWhile (\x -> x /= "--") args


