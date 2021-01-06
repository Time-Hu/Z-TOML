{-|
Module      : Z.Data.JSON.Builder
Description : JSON representation and builders
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides builders for JSON 'Value's, a Haskell JSON representation. These builders are designed to comply with <https://tools.ietf.org/html/rfc8258 rfc8258>. Only control characters are escaped, other unicode codepoints are directly written instead of being escaped.

-}
module Z.Data.TOML.Builder where

import           Control.Monad
import           Data.Word ( Word8 ) 
import qualified Z.Data.Builder                 as B
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Print              as T
import           Z.Data.Vector.Base             as V
import           Z.Data.TOML.Parser 
import           Z.Data.TOML.Value 
import           Data.Scientific              (Scientific, base10Exponent, coefficient, fromFloatDigits)

toml :: Toml -> B.Builder()
toml (TKeyV kvs) = sequence_ (kv <$> kvs) 
toml (Table tk kvs) = tableKey tk >> newlineB >> sequence_ (kv <$> kvs)

tableKey :: TableKey -> B.Builder ()
tableKey (StdKey tk) = B.square (key tk)
tableKey (AKey tk) = B.square (B.square (key tk))

key :: [T.Text] -> B.Builder()
key = foldr f (return ())
     where 
        f x y = string x >> B.encodePrim @Word8 46 >> y

newlineB :: B.Builder ()
newlineB = B.encodePrim @Word8 0X0A

equalB :: B.Builder () 
equalB = B.encodePrim @Word8 61

-- | Use @:@ as separator to connect a label(no need to escape, only add quotes) with field builders.
-- kv :: T.Text -> Value -> B.Builder ()
-- {-# INLINE kv #-}
-- l `kv` b = B.quotes (B.text l) >> equalB >> value b >> newlineB

-- | Use @:@ as separator to connect a label(escaped and add quotes) with field builders.
kv :: ([T.Text], Value) -> B.Builder ()
{-# INLINE kv #-}
kv (xs, y) = key xs >> equalB >> value y >> newlineB

-- | Encode a 'Value', you can use this function with 'toValue' to get 'encodeJSON' with a small overhead.
value :: Value -> B.Builder ()
{-# INLINABLE value #-}
value (TInlineTable kvs) = object (pack kvs)
value (TArray        vs) = array (pack vs)
value (TString        t) = string t
value (TInteger       n) = scientific (fromInteger n)
value (TDouble        n) = scientific (fromFloatDigits n) 
value (TTime          t) = B.text (T.pack . show $ t)
value (TBool       True) = "true"
value (TBool      False) = "false"

array :: V.Vector Value -> B.Builder ()
{-# INLINE array #-}
array = B.square . B.intercalateVec B.comma value

array' :: (a -> B.Builder ()) -> V.Vector a -> B.Builder ()
{-# INLINE array' #-}
array' f = B.square . B.intercalateVec B.comma f

object :: V.Vector ([T.Text], Value) -> B.Builder ()
{-# INLINE object #-}
object = B.curly . B.intercalateVec B.comma kv

-- object' :: (a -> B.Builder ()) -> V.Vector ([T.Text], a) -> B.Builder ()
-- {-# INLINE object' #-}
-- object' f = B.curly . B.intercalateVec B.comma kv

string :: T.Text -> B.Builder ()
{-# INLINE string #-}
string = T.escapeTextJSON

-- | This builder try to render integer when (0 <= e < 1024), and scientific notation otherwise.
scientific :: Scientific -> B.Builder ()
{-# INLINE scientific #-}
scientific s
    | e < 0 || e >= 1024 = B.scientific s
    | e == 0 = B.integer c
    | otherwise = do
        B.integer c
        when (c /= 0) (replicateM_ e (B.encodePrim @Word8 48))
  where
    e = base10Exponent s
    c = coefficient s
