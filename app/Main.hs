module Main (main) where

import Relude

import Pixy.Data
import Pixy
import Fmt((+|), (|+))
import qualified Fmt

{- [markdown]
# About DataLog


-}

main :: IO ()
main =
  do
    let scriptText = ""+|script|+""
    Fmt.fmtLn  scriptText
    case evalScript script of
      Left errs -> traverse_
        (\e -> Fmt.fmtLn (""+|e|+"")) errs
      Right db -> Fmt.fmtLn (""+|db|+"")

-- definitions

