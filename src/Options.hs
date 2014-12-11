module Options (Options(..), defaultOptions, applyOptions) where
  import Arguments (name, value)

  data Options = Options {
    printBack :: Int
  } deriving (Show)

  defaultOptions = Options {
    printBack = 10
  }

  applyOptions [] = defaultOptions
  applyOptions args =
    let
      applyArguments [] options = options
      applyArguments (currentArg:rest) options@(Options { printBack = pb }) =
        case (name currentArg) of
          "-n"      -> applyArguments rest (applyPrintBackOption options (value currentArg))
          "--back"  -> applyArguments rest (applyPrintBackOption options (value currentArg))
          "-a"      -> applyArguments rest (applyAllOption options)
          "--all"   -> applyArguments rest (applyAllOption options)
          otherwise -> applyArguments rest (Options { printBack = pb })
    in
      applyArguments args defaultOptions

  applyPrintBackOption (Options { printBack = pb }) value = (Options { printBack = read value::Int })

  applyAllOption (Options { printBack = pb }) = (Options { printBack = -1 })
