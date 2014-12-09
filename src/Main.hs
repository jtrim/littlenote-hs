module Main where
  import System.Environment  (getArgs)
  import Data.Time.LocalTime (getZonedTime)
  import Data.Time.Format    (formatTime, defaultTimeLocale)
  import Data.List           (intercalate)

  main = do
    args <- getArgs
    processArgs args

  processArgs [] = printNotes
  processArgs args = do
    addition <- formatNoteLine $ getIncomingNote args
    appendFile notesFilePath addition

  printNotes = do
    notes <- readFile notesFilePath
    putStr $ unlines $ reverse $ take 10 $ reverse $ lines notes

  currentFormattedTime = do
    t <- getZonedTime
    return (formatTime defaultTimeLocale "%D %r" t)

  formatNoteLine s = do
    formattedTime <- currentFormattedTime
    return $ formattedTime ++ " - " ++ s ++ "\n"

  getIncomingNote []   = ""
  getIncomingNote [x]  = x
  getIncomingNote args = let
    postDashyArgs = tail $ dropWhile (\x -> x /= "--") args
    in
      messageFrom postDashyArgs
      where
        messageFrom []    = head args
        messageFrom args' = intercalate " " args'

  notesFilePath = "./littlenotes.txt"
