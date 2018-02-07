-- | Core delegation types.
module Pos.Core.Delegation
       (
         LightDlgIndices (..)
       , ProxySigLight
       , ProxySKLight

       , HeavyDlgIndex (..)
       , ProxySigHeavy
       , ProxySKHeavy

       , DlgPayload (..)
       , DlgProof
       , mkDlgProof
       , checkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Default (Default (def))
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, (%))
import           Serokell.Util (listJson, pairF)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Crypto (Hash, ProxySecretKey (..), ProxySignature, hash)
import           Pos.Crypto.Configuration (HasCryptoConfiguration)

----------------------------------------------------------------------------
-- Proxy signatures and signing keys
----------------------------------------------------------------------------

-- Notice: light delegation was removed as part of CSL-1856 and should
-- be reworked later. Though some parts of it are left to support
-- backward compatibility.

-- | Pair of indices for light delegation PSK that define start and
-- end epoch of cert usage. Block is valid if its epoch index is
-- inside this range.
data LightDlgIndices =
    LightDlgIndices { getLightDlgIndices :: (EpochIndex, EpochIndex) }
    deriving (Show, Eq, Ord, Generic)

instance NFData LightDlgIndices

instance Buildable LightDlgIndices where
    build (LightDlgIndices p) = bprint pairF p

-- | Light delegation proxy signature, that holds a pair of epoch
-- indices.
type ProxySigLight a = ProxySignature LightDlgIndices a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey LightDlgIndices


-- | Witness for heavy delegation signature -- epoch in which
-- certificate starts being active. It is needed for replay attack
-- prevention (index should match epoch of the block PSK is announced
-- in).
data HeavyDlgIndex =
    HeavyDlgIndex { getHeavyDlgIndex :: EpochIndex }
    deriving (Show, Eq, Ord, Generic)

instance NFData HeavyDlgIndex

instance Buildable HeavyDlgIndex where
    build (HeavyDlgIndex i) = bprint build i

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigHeavy a = ProxySignature HeavyDlgIndex a

-- | Heavy delegation PSK.
type ProxySKHeavy = ProxySecretKey HeavyDlgIndex

----------------------------------------------------------------------------
-- Payload
----------------------------------------------------------------------------

-- | 'DlgPayload' is put into 'MainBlock' and is a set of heavyweight
-- proxy signing keys.
newtype DlgPayload = DlgPayload
    { getDlgPayload :: Set ProxySKHeavy
    } deriving (Show, Eq, Generic, NFData)

instance Default DlgPayload where
    def = DlgPayload mempty

instance Buildable DlgPayload where
    build (DlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (S.size psks) (toList psks)

checkDlgPayload
    :: (HasCryptoConfiguration, MonadError Text m)
    => DlgPayload
    -> m ()
checkDlgPayload (DlgPayload x) = forM_ x undefined

-- | Proof of delegation payload.
type DlgProof = Hash DlgPayload

-- | Creates 'DlgProof' out of delegation payload.
mkDlgProof :: Bi DlgPayload => DlgPayload -> DlgProof
mkDlgProof = hash
