module Littlenote.Context where

  data Context = Context {
    notePath :: Maybe String
  } deriving (Show)

  defaultContext = Context {
    notePath = Nothing
  }

  withNotePath path context = context { notePath = Just path }
