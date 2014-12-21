module Options (Options(..), defaultOptions, applyOptions) where
  import Arguments (name, value)

  data Options = Options {
    printBack :: Int,
    dateFilter :: Maybe String
  } deriving (Show)

  defaultOptions = Options {
    printBack = 10,
    dateFilter = Nothing
  }

  applyOptions [] = defaultOptions
  applyOptions args =
    let
      applyArguments [] options = options
      applyArguments (currentArg:rest) options =
        case (name currentArg) of
          "-n"      -> applyArguments rest (setPrintBack options (value currentArg))
          "--back"  -> applyArguments rest (setPrintBack options (value currentArg))
          "-a"      -> applyArguments rest (setAll options)
          "--all"   -> applyArguments rest (setAll options)
          "-d"      -> applyArguments rest (setDateFilter options (value currentArg))
          otherwise -> applyArguments rest options
    in
      applyArguments args defaultOptions

  setAll options = options { printBack = -1 }

  setPrintBack options value = options { printBack = read value::Int }

  setDateFilter options value = options { dateFilter = Just value }
