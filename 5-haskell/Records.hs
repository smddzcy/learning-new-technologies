module Records where

data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , height      :: Float
                     , phoneNumber :: String
                     } deriving (Show)

{- Haskell automatically creates some utility functions for the record.
   firstName :: Person -> String
   lastName :: Person -> String
   age :: Person -> String ... -}

me :: Person
me = Person { firstName = "Samed"
            , lastName = "Düzçay"
            , age = 20
            , height = 1.80
            , phoneNumber = "(554) 000 11 22"
            }

-- Ord is derived based on the definition order. Sunday > all.
-- Read is opposite of show. read "Monday" == Monday.
-- Note: read "Monday" gives an error since Haskell cannot know its type.
-- Give the type explicitly: read "Monday" :: Day
-- Enum means this can be enumerated.
-- Note: Day is a type constructor and right-hand side contains value constructors.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Bounded => minBound :: Day == Monday   -   maxBound :: Day == Sunday
-- Enum    => succ Monday == Tuesday   -   pred Tuesday == Monday
