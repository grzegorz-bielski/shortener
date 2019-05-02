module Lib
    ( generateHash
    ) where

import           Control.Monad (replicateM)
import qualified System.Random as SR

generateHash :: IO String
generateHash = hash
    where hash = replicateM num $ randomElement alphaNum
          num = 7

          alphaNum = ['A'..'Z'] <> ['0'..'9']

          randomElement xs =
            let getElement randomDigit = xs !! randomDigit
            in
            getElement <$> SR.randomRIO (0, length xs - 1)
