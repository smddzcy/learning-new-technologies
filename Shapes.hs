module Shapes
  ( Point(..)
  , Shape(..)
  , area
  , nudge
  , baseCircle
  , baseRect
  ) where

data Point = Point Float Float deriving (Show, Eq)

data Shape = Circle Point Float
           | Rectangle Point Point
           | Line Point Point
           | Triangle Point Point Point deriving (Show, Eq)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)
area (Line (Point x1 y1) (Point x2 y2)) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
area (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 0.5 * abs ((x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) a b = Point (x + a) (y + b)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle p r) a b = Circle (nudgePoint p a b) r
nudge (Rectangle p1 p2) a b = Rectangle (nudgePoint p1 a b) (nudgePoint p2 a b)
nudge (Line p1 p2) a b = Line (nudgePoint p1 a b) (nudgePoint p2 a b)
nudge (Triangle p1 p2 p3) a b = Triangle (nudgePoint p1 a b) (nudgePoint p2 a b) (nudgePoint p3 a b)
