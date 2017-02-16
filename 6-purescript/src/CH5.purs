module CH5 where

import Prelude

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: forall t1 t2.               
  { "city" :: String | t1  } ->
  { "city" :: String | t2 } ->
  Boolean
sameCity { city: c1 } { city: c2 } = c1 == c2


livesInLA :: forall t1 t2. { address :: { city :: String | t1 } | t2} -> Boolean
livesInLA { address: { city: "Los Angeles" }} = true
livesInLA _ = false


fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default

