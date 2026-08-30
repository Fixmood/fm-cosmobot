{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Bot.Util.HList
Description : Tiny typed heterogeneous context
Stability   : experimental
-}

module Bot.Util.HList
  ( HList (..)
  , Has (..)
  , Put (..)
  )
where

import Bot.Prelude hiding (get, put)

infixr 5 :&
data HList fields where
  HNil :: HList '[]
  (:&) :: field -> HList rest -> HList (field ': rest)

class Has field (fields :: [Type]) where
  get :: HList fields -> field

instance Has field (field ': rest) where
  get (field :& _) =
    field

instance {-# OVERLAPPABLE #-} Has field rest => Has field (other ': rest) where
  get (_ :& rest) =
    get @field rest

class Put field (fields :: [Type]) where
  put :: field -> HList fields -> HList fields

instance Put field (field ': rest) where
  put field (_ :& rest) =
    field :& rest

instance {-# OVERLAPPABLE #-} Put field rest => Put field (other ': rest) where
  put field (other :& rest) =
    other :& put field rest
