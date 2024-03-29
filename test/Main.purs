module Test.Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, pure, show, unit, negate, ($), (<>), (==))
import Proto.Uint8Array (Uint8Array, fromArray)
import Data.Either (Either(Left, Right))
import Node.Process (exit)
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.BigInt (BigInt)
import Proto.BigInt (fromNumber, fromString) as BigInt

main :: Effect Unit
main = do
  case_int_zero
  case_int_max
  case_int_min
  case_long_maxint
  case_long_minint
  case_eq
  case_bigint_max_number
  case_bigint_min_number
  case_bigint_max_long
  case_bigint_min_long
  case_bigint_zero

case_eq :: Effect Unit
case_eq = do
  let xs = fromArray []
  let ys = fromArray []
  if xs == ys then log "case_eq: ok"
  else do
    log "case_eq: failed"
    exit 99

case_int_zero :: Effect Unit
case_int_zero = do
  let x = 0
  let encoded = Encode.signedVarint32 x
  let decoded = Decode.signedVarint32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 1
    Right { val } -> assertEqual val x
  log "case_int_zero: ok"

case_int_max :: Effect Unit
case_int_max = do
  let x = 2147483647
  let encoded = Encode.signedVarint32 x
  let decoded = Decode.signedVarint32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 2
    Right { val } -> assertEqual val x
  log "case_int_max: ok"

case_int_min :: Effect Unit
case_int_min = do
  let x = -2147483648
  let encoded = Encode.signedVarint32 x
  let decoded = Decode.signedVarint32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 3
    Right { val } -> assertEqual val x
  log "case_int_min: ok"

case_long_maxint :: Effect Unit
case_long_maxint = do
  let x = 9007199254740991.0 -- Number.MAX_SAFE_INTEGER
  let encoded = Encode.signedVarint64 x
  let decoded = Decode.signedVarint64 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 4
    Right { val } -> assertEqual val x
  log "case_long_maxint: ok"

case_long_minint :: Effect Unit
case_long_minint = do
  let x = -9007199254740991.0 -- Number.MIN_SAFE_INTEGER
  let encoded = Encode.signedVarint64 x
  let decoded = Decode.signedVarint64 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 5
    Right { val } -> assertEqual val x
  log "case_long_minint: ok"

case_bigint_max_number :: Effect Unit
case_bigint_max_number = 
  case_bigint "case_bigint_max_number" $ BigInt.fromNumber 9007199254740991.0

case_bigint_min_number :: Effect Unit
case_bigint_min_number =
  case_bigint "case_bigint_min_number" $ BigInt.fromNumber $ -9007199254740991.0

case_bigint_max_long :: Effect Unit
case_bigint_max_long = 
  case_bigint "case_bigint_max_long" $ BigInt.fromString "9223372036854775807"

case_bigint_min_long :: Effect Unit
case_bigint_min_long = 
  case_bigint "case_bigint_min_long" $ BigInt.fromString "-9223372036854775807"

case_bigint_zero :: Effect Unit
case_bigint_zero = 
  case_bigint "case_bigint_zero" $ BigInt.fromNumber 0.0

case_bigint :: String -> BigInt -> Effect Unit
case_bigint name x = do
  let encoded = Encode.bigInt x
  let decoded = Decode.bigInt encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 5
    Right { val } -> assertEqual val x
  log $ name <> ": ok"

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual actual expected =
  if actual == expected then pure unit
  else do
    log $ "  actual: " <> show actual
    log $ "expected: " <> show expected
    exit 99

foreign import printBytes :: Uint8Array -> String
