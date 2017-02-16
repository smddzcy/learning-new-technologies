module Data.Shape where

import Prelude
import Data.Maybe (Maybe(..))

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point { x :: Number, y:: Number }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r) = show c <> " - r:" <> show r
  show (Rectangle center w h) = show center <> " - w:" <> show w <> ", " <> "h:" <> show h
  show (Line s e) = "Line [start: " <> show s <> " - " <> "end: " <> show e <> "]"
  show (Text loc text) = show loc <> " - text:'" <> text <> "'"

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

double :: Shape -> Shape
double (Circle c r) = Circle origin (2.0 * r)
double (Rectangle center w h) = Rectangle center (2.0 * w) (2.0 * h)
double (Line s (Point e)) = Line s $ Point { x: 2.0 * e.x, y: 2.0 * e.y }
double (Text (Point loc) text) = Text (Point { x: 2.0 * loc.x, y: 2.0 * loc.y }) text

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing



