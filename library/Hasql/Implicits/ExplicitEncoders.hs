{-|
Extensions to the explicit encoders DSL from the \"hasql\" library ("Hasql.Encoders").
-}
module Hasql.Implicits.ExplicitEncoders where

import Hasql.Implicits.Prelude
import Hasql.Encoders as Old


{-|
Lift a single-param encoder definition to the level of composable encoder of multiple params.
-}
param :: Param a -> Params a
param (Param params) = params


{-|
Single param encoder, which captures its nullability.
-}
newtype Param a = Param (Params a)

{-|
Lift a value encoder into a non-nullable parameter encoder.
-}
nonNullable :: Value a -> Param a
nonNullable = Param . Old.param

{-|
Lift a value encoder into a nullable parameter encoder.
-}
nullable :: Value a -> Param (Maybe a)
nullable = Param . nullableParam

