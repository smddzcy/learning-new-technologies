module Regex where

import Prelude
import Data.Unit
import Data.String.Regex.Flags
import Data.String.Regex
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)


matches :: String -> Regex -> Maybe (Array (Maybe String))
matches str reg = match reg str

main :: forall t. Eff ( "console" :: CONSOLE | t ) Unit
main = do
  let flags = RegexFlags { global: true, ignoreCase: true, multiline: true, unicode: false, sticky: false }
  let a = regex "[0-5][3-8]" flags
  case a of
    Left b -> log b
    Right c -> do
                let matched = match c "555 44 33 10 399 001 38 19"
                case matched of
                  Nothing -> log "No match."
                  Just d -> void $ log `traverse` catMaybes d
