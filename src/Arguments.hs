module Arguments (
  Argument(..),
  name,
  value,
  determineArguments
) where

  import Data.List (isPrefixOf)

  data Argument = BooleanArgument String
                | ValueArgument   String String
                | CommandArgument String
                deriving (Show)

  name :: Argument -> String
  name (BooleanArgument n) = n
  name (ValueArgument n _) = n
  name (CommandArgument n) = n

  value :: Argument -> String
  value a@(BooleanArgument _) = name a
  value (ValueArgument _ val) = val
  value a@(CommandArgument _) = name a

  determineArguments :: [String] -> [Argument]
  determineArguments [] = []
  determineArguments (arg:nextArg:args)
    | isArgumentTerminator arg = []
    | otherwise =
      case buildArgument (Just arg) (Just nextArg) of
        argument@(BooleanArgument _) -> argument:determineArguments (nextArg:args)
        argument@(CommandArgument _) -> argument:determineArguments (nextArg:args)
        argument@(ValueArgument _ _) -> argument:determineArguments args
  determineArguments (arg:args) = buildArgument (Just arg) Nothing:determineArguments args

  buildArgument :: Maybe String -> Maybe String -> Argument
  buildArgument (Just current) Nothing
    | isDashy current = BooleanArgument current
    | otherwise       = CommandArgument current
  buildArgument (Just current) (Just next)
    | isDashy current = if isDashy next
                        then BooleanArgument current
                        else ValueArgument   current next
    | otherwise       = CommandArgument current

  isArgumentTerminator = (==) "--"
  isDashy              = isPrefixOf "-"
