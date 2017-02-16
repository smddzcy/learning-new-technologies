module FileOperations where

import Prelude
import Data.Traversable
import Data.Maybe
import Data.Path
import Control.MonadZero (guard)
import Data.Array (filter, concatMap, (:), head)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles path = filter isFile $ allFiles path

largestFile :: Path -> Path
largestFile path = foldl (\acc xs -> largerFile acc xs) emptyFile $ onlyFiles path
  where
    largerFile :: Path -> Path -> Path
    largerFile = \acc xs -> if size xs > size acc then xs else acc

  
whereIs :: String -> Maybe Path
whereIs name = head $ do
  dir <- filter isDirectory $ allFiles root
  file <- filter isFile $ ls dir
  guard $ filename file == name
  pure dir
  
  

