module Main where

import Prelude
import Data.Maybe
import Data.List
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.AddressBook
import Control.Monad.Eff.Exception (EXCEPTION, Error, throw, throwException)
import Control.Monad.Eff.Random (random)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import Data.Array (tail, head)
import React

third :: forall a. Array a -> Maybe a
third a = tail a >>= tail >>= head

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM fn Nil = pure Nil
filterM fn (Cons x xs) = fn x >>= (\b -> if b then (filterM fn xs >>= (pure <<< Cons x)) else filterM fn xs)

safeDivide :: Int -> Int -> Eff ("err" :: EXCEPTION) Int
safeDivide nom 0 = throw "Denominator cannot be 0"
safeDivide nom denom = pure $ nom / denom

main :: Eff (console :: CONSOLE, dom :: DOM) Unit
main = do
  doc <- window >>= document
  log "Rendering address book component"


