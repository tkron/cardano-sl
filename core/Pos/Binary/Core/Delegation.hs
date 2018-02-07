-- | Delegation types serialization.

module Pos.Binary.Core.Delegation () where

import           Nub (ordNub)
import           Universum

import qualified Data.Set as S

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Core.Slotting ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Delegation (DlgPayload (..), HeavyDlgIndex (..), LightDlgIndices (..),
                                      ProxySKHeavy)
import           Pos.Util.Verification (Unver)

instance Bi HeavyDlgIndex where
    encode = encode . getHeavyDlgIndex
    decode = HeavyDlgIndex <$> decode

instance Bi LightDlgIndices where
    encode = encode . getLightDlgIndices
    decode = LightDlgIndices <$> decode

instance Bi DlgPayload where
    encode = encode . S.toList . getDlgPayload
    decode = do
        (psks :: [Unver ProxySKHeavy]) <- decode
        let asSet :: Unver (Set ProxySKHeavy)
            asSet = S.fromList <$> (sequence psks :: Unver [ProxySKHeavy])
        when (length psks /= length (ordNub psks)) $
              fail "DlgPayload is not a set: it has duplicates"
        pure $ UnsafeDlgPayload <$> asSet
