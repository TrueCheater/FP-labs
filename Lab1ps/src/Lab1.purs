module Lab1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), reverse, drop)

infixr 6 Cons as :

singleton :: forall a. a -> List a
singleton a = a : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc list a = reverse $ a : reverse list

length :: forall a. List a -> Int
length Nil = 0
length list = 1 + length (drop 1 list)

test :: Effect Unit
test = do
    log $ show $ singleton (1)
    log $ show $ null ("x" : "y" : Nil)
    log $ show $ null Nil
    log $ show $ snoc ("1" : "2" : Nil) ("3")
    log $ show $ snoc Nil (5)
    log $ show $ length ("1" : "6" : "3" : "9" : Nil)
    log $ show $ length Nil
