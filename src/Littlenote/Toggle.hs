module Littlenote.Toggle (toggles, extractToggle, KnownToggle(..)) where
  import Data.List (find, takeWhile)
  import Data.List.Split (splitOn)

  type Short = String
  type Long = String
  type ToggleIdentifier = String
  type ToggleValue = String

  data KnownToggle = BooleanToggle Short Long
                   | ValueToggle   Short Long
                   deriving (Show, Eq)

  type UserToggle = (KnownToggle, (Maybe ToggleValue))

  toggles :: [String] -> [KnownToggle] -> [UserToggle]
  toggles arguments knownToggles = foldr appendUserToggle [] pairs
    where
      appendUserToggle :: (ToggleIdentifier, ToggleValue) -> [UserToggle] -> [UserToggle]
      appendUserToggle pair suppliedToggles = case extractToggle pair knownToggles of
        Nothing -> suppliedToggles
        Just t  -> t:suppliedToggles
      pairs = zip validArgs $ tail validArgs ++ [""]
        where
          isDashy "--"  = True
          isDashy _     = False
          processedArgs = concat $ map (\x -> splitOn "=" x) arguments
          validArgs     = takeWhile (not . isDashy) processedArgs

  extractToggle :: (ToggleIdentifier, ToggleValue) -> [KnownToggle] -> Maybe UserToggle
  extractToggle (identifier, value) knownToggles =
    fmap (pairToggleWithValue value) $ find (identifierMatchesToggle identifier) knownToggles

  pairToggleWithValue value toggle  = case toggle of
    (BooleanToggle _ _) -> (toggle, Nothing)
    (ValueToggle   _ _) -> (toggle, Just value)

  identifierMatchesToggle identifier toggle = identifier == short || identifier == long
    where
    (short, long) = toggleIdentifiers toggle

  toggleIdentifiers (BooleanToggle short long) = (short, long)
  toggleIdentifiers (ValueToggle   short long) = (short, long)
