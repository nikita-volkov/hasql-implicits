{-# LANGUAGE CPP #-}
module Hasql.Implicits.Encoders where

import Hasql.Implicits.Prelude
import Hasql.Encoders
import qualified Data.Aeson as Aeson


{-| Provides a default implementation of value encoder. -}
class DefaultValueEncoder a where
  {-| Default value encoder with nullability specified. -}
  defaultValue :: NullableOrNot Value a

#define RIGHT_QUOTED(a) a'
#define INSTANCES(VALUE, ENCODER) \
instance DefaultValueEncoder VALUE where { \
  defaultValue = nonNullable ENCODER; \
}; \
instance DefaultValueEncoder [VALUE] where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder [Maybe VALUE] where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder [[VALUE]] where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder [[Maybe VALUE]] where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Vector VALUE) where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Vector (Maybe VALUE)) where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Vector (Vector VALUE)) where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Vector (Vector (Maybe VALUE))) where { \
  defaultValue = (nonNullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe VALUE) where { \
  defaultValue = nullable ENCODER; \
}; \
instance DefaultValueEncoder (Maybe [VALUE]) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe [Maybe VALUE]) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe [[VALUE]]) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe [[Maybe VALUE]]) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe (Vector VALUE)) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe (Vector (Maybe VALUE))) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe (Vector (Vector VALUE))) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nonNullable) ENCODER; \
}; \
instance DefaultValueEncoder (Maybe (Vector (Vector (Maybe VALUE)))) where { \
  defaultValue = (nullable . array . dimension RIGHT_QUOTED(foldl) . dimension RIGHT_QUOTED(foldl) . element . nullable) ENCODER; \
}

INSTANCES(Char, char)
INSTANCES(Double, float8)
INSTANCES(Float, float4)
INSTANCES(Int16, int2)
INSTANCES(Int32, int4)
INSTANCES(Int64, int8)
INSTANCES(ByteString, bytea)
INSTANCES(Scientific, numeric)
INSTANCES(Text, text)
INSTANCES(UTCTime, timestamptz)
INSTANCES(Aeson.Value, jsonb)
INSTANCES(UUID, uuid)
INSTANCES(Day, date)
INSTANCES(DiffTime, interval)
INSTANCES(TimeOfDay, time)
INSTANCES(LocalTime, timestamp)
INSTANCES((TimeOfDay, TimeZone), timetz)
INSTANCES((NetAddr IP), inet)

#undef INSTANCES
#undef RIGHT_QUOTED


{-| Parameter encoder derived from its input type. -}
defaultParam :: DefaultValueEncoder param => Params param
defaultParam = param defaultValue

{-| Array element encoder derived from its input type. -}
defaultElement :: DefaultValueEncoder element => Array element
defaultElement = element defaultValue
