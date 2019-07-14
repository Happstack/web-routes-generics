{-# language PolyKinds, TypeOperators, DeriveGeneric, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
module Web.Routes.Generics where

import Data.Text (Text, pack, toLower)
import GHC.Generics
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Web.Routes.PathInfo (PathInfo(fromPathSegments, toPathSegments), URLParser, segment)

class GToURL f where
  gtoPathSegments :: f a -> [Text]
  gfromPathSegments :: URLParser (f a)

instance GToURL U1 where
  gtoPathSegments U1 = []
  gfromPathSegments = eof >> pure U1

instance forall c f. (Constructor c, GToURL f) => GToURL (C1 c f) where
  gtoPathSegments m@(M1 x) = (toLower $ pack $ conName m) : gtoPathSegments x
  gfromPathSegments =
    let constr = undefined :: C1 c f r
    in do segment (toLower $ pack $ conName constr) <|> segment (pack $ conName constr)
          M1 <$> gfromPathSegments

instance (GToURL f, GToURL g) => GToURL (f :+: g) where
  gtoPathSegments (L1 x) = gtoPathSegments x
  gtoPathSegments (R1 x) = gtoPathSegments x
  gfromPathSegments = try (L1 <$> gfromPathSegments) <|> (R1 <$> gfromPathSegments)

instance (GToURL f, GToURL g) => GToURL (f :*: g) where
  gtoPathSegments (x :*: y) = gtoPathSegments x ++ gtoPathSegments y
  gfromPathSegments =
    do x <- gfromPathSegments
       y <- gfromPathSegments
       pure (x :*: y)

instance (GToURL f) => GToURL (D1 c f) where
  gtoPathSegments m@(M1 x) = gtoPathSegments x
  gfromPathSegments = M1 <$> gfromPathSegments

instance (PathInfo a) => GToURL (K1 i a) where
  gtoPathSegments (K1 a) = toPathSegments a
  gfromPathSegments = K1 <$> fromPathSegments

instance (GToURL f) => GToURL (S1 c f) where
  gtoPathSegments (M1 f) = gtoPathSegments f
  gfromPathSegments = M1 <$> gfromPathSegments
