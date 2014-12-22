module Littlenote.Note (Note(..), parse, render) where
  import Text.Regex.Posix ((=~))
  import Data.Time.LocalTime (LocalTime)
  import qualified Littlenote.Date as D

  data Note = Note {
    text :: String,
    recordedAt :: LocalTime
  } deriving (Show, Eq)

  parse :: String -> Note
  parse input = let
    (dateString, _, noteText) = consume input
    date = D.parse dateString
    in
      Note { recordedAt = date, text = noteText }
      where consume input = input =~ " - " :: (String, String, String)


  render :: Note -> String
  render (Note {
            recordedAt = recordedAt,
            text       = text
          }) = D.format recordedAt ++ " - " ++ text
