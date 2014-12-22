module Littlenote.Date (format, parse) where
  import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
  import Data.Time.LocalTime (LocalTime(..))
  import Data.Maybe (fromJust)

  formatString = "%m/%d/%Y %I:%M:%S %p"

  format :: LocalTime -> String
  format = formatTime defaultTimeLocale formatString

  parse :: String -> LocalTime
  parse t = let
    parsed = parseTimeM True defaultTimeLocale formatString t :: Maybe LocalTime
    in
      fromJust parsed

