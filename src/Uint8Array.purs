module Proto.Uint8Array where

import Data.Either (Either(Left, Right))
import Prelude

foreign import data Uint8Array :: Type

foreign import length :: Uint8Array -> Int
foreign import indexUnsafe :: Uint8Array -> Int -> Int
foreign import slice :: Uint8Array -> Int -> Int -> Uint8Array
foreign import concatAll :: Array Uint8Array -> Uint8Array
foreign import fromArray :: Array Int -> Uint8Array

foreign import eqImpl :: Uint8Array -> Uint8Array -> Boolean

instance eqUint8Array :: Eq Uint8Array where
  eq = eqImpl

index :: Uint8Array -> Int -> Either { pos :: Int, len :: Int } Int
index xs pos =
  let len = length xs
  in if 0 <= pos && pos < len then Right (indexUnsafe xs pos) else Left { pos, len }

