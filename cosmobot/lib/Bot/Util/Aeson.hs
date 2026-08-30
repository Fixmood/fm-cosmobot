{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Bot.Util.Aeson
Description : Small deriving-via wrappers for Aeson generic instances.
Stability   : experimental
-}

module Bot.Util.Aeson
  ( SnakeJSON (..)
  , SnakeJSONOmitNothing (..)
  , PrefixedSnakeJSON (..)
  , PrefixedSnakeJSONOmitNothing (..)
  , PrefixedEnumJSON (..)
  , JSON (..)
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.List (dropWhileEnd, stripPrefix)
import GHC.Generics (Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

newtype SnakeJSON a = SnakeJSON a

newtype SnakeJSONOmitNothing a = SnakeJSONOmitNothing a

newtype JSON a = JSON a

newtype PrefixedSnakeJSON (prefix :: Symbol) a = PrefixedSnakeJSON a

newtype PrefixedSnakeJSONOmitNothing (prefix :: Symbol) a = PrefixedSnakeJSONOmitNothing a

newtype PrefixedEnumJSON (prefix :: Symbol) a = PrefixedEnumJSON a

instance
  ( Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (JSON a) where
  toJSON (JSON value) =
    Aeson.genericToJSON Aeson.defaultOptions value

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (JSON a) where
  parseJSON =
    fmap JSON . Aeson.genericParseJSON Aeson.defaultOptions

instance
  ( Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (SnakeJSON a) where
  toJSON (SnakeJSON value) =
    Aeson.genericToJSON snakeOptions value

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (SnakeJSON a) where
  parseJSON =
    fmap SnakeJSON . Aeson.genericParseJSON snakeOptions

instance
  ( Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (SnakeJSONOmitNothing a) where
  toJSON (SnakeJSONOmitNothing value) =
    Aeson.genericToJSON snakeOmitNothingOptions value

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (SnakeJSONOmitNothing a) where
  parseJSON =
    fmap SnakeJSONOmitNothing . Aeson.genericParseJSON snakeOmitNothingOptions

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (PrefixedSnakeJSON prefix a) where
  toJSON (PrefixedSnakeJSON value) =
    Aeson.genericToJSON (prefixedSnakeOptions @prefix) value

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (PrefixedSnakeJSON prefix a) where
  parseJSON =
    fmap PrefixedSnakeJSON . Aeson.genericParseJSON (prefixedSnakeOptions @prefix)

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (PrefixedSnakeJSONOmitNothing prefix a) where
  toJSON (PrefixedSnakeJSONOmitNothing value) =
    Aeson.genericToJSON (prefixedSnakeOmitNothingOptions @prefix) value

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (PrefixedSnakeJSONOmitNothing prefix a) where
  parseJSON =
    fmap PrefixedSnakeJSONOmitNothing . Aeson.genericParseJSON (prefixedSnakeOmitNothingOptions @prefix)

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
  )
  => Aeson.ToJSON (PrefixedEnumJSON prefix a) where
  toJSON (PrefixedEnumJSON value) =
    Aeson.genericToJSON (prefixedEnumOptions @prefix) value

instance
  ( Generic a
  , KnownSymbol prefix
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  )
  => Aeson.FromJSON (PrefixedEnumJSON prefix a) where
  parseJSON =
    fmap PrefixedEnumJSON . Aeson.genericParseJSON (prefixedEnumOptions @prefix)

snakeOptions :: Aeson.Options
snakeOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = snakeFieldLabel
    }

snakeOmitNothingOptions :: Aeson.Options
snakeOmitNothingOptions =
  snakeOptions
    { Aeson.omitNothingFields = True
    }

prefixedSnakeOptions :: forall prefix. KnownSymbol prefix => Aeson.Options
prefixedSnakeOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = snakeFieldLabel . dropFieldPrefix (symbolVal (Proxy @prefix))
    }

prefixedSnakeOmitNothingOptions :: forall prefix. KnownSymbol prefix => Aeson.Options
prefixedSnakeOmitNothingOptions =
  (prefixedSnakeOptions @prefix)
    { Aeson.omitNothingFields = True
    }

prefixedEnumOptions :: forall prefix. KnownSymbol prefix => Aeson.Options
prefixedEnumOptions =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = snakeFieldLabel . dropFieldPrefix (symbolVal (Proxy @prefix))
    }

dropFieldPrefix :: String -> String -> String
dropFieldPrefix "" field =
  field
dropFieldPrefix prefix field =
  case stripPrefix prefix field of
    Just suffix ->
      lowerInitial suffix
    Nothing ->
      field

lowerInitial :: String -> String
lowerInitial = \case
  [] ->
    []
  char : rest ->
    Char.toLower char : rest

snakeFieldLabel :: String -> String
snakeFieldLabel =
  Aeson.camelTo2 '_' . dropWhileEnd (== '_')
