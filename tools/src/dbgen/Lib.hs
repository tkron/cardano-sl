{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib where

import           Universum

import           CLI
import           Types (UberMonad)
import           Data.Function                        (id)
import qualified Data.ByteString                      as B
import           Data.String.Conv
import           Data.Time
import           Data.Map                             (union, fromList)
--import           Dhall                                (Interpret, input, auto)
import           Data.Aeson                           (FromJSON, ToJSON, eitherDecodeStrict)
import           GHC.Generics                         (Generic)
import           Network.Haskoin.Crypto               (Entropy, toMnemonic)
import           Pos.Crypto.Random
import           Pos.DB.GState.Common                 (getTip)
import           Pos.StateLock
import           Pos.Core.Address                     (Address)
import           Pos.Core.Types                       (Coin, mkCoin)
import           Pos.Txp.Core.Types                   (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types                   (utxoToModifier)
import           Pos.Util.BackupPhrase
import           Pos.Util.Servant
import           Pos.Util.Util                        (lensOf)
import           Pos.Wallet.Web.Account
import           Pos.Wallet.Web.ClientTypes
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.Methods.Logic
import           Pos.Wallet.Web.Methods.Restore
import           Pos.Wallet.Web.State.State           (getWalletUtxo, setWalletUtxo, updateWalletBalancesAndUtxo)
import           Test.QuickCheck                      (arbitrary, generate)
import           Rendering
import           Text.Printf

--
-- Types
--

-- | A simple example of how the configuration looks like.
_testSpec :: GenSpec
_testSpec = GenSpec
    { walletSpec = WalletSpec
        { accounts = 1
        , accountSpec = AccountSpec { addresses = 100 }
        , fakeUtxo = FakeUtxoSpec { amount = 1000, distribution = RangeDistribution { range=100 } }
        }
    , wallets = 1
    }

data GenSpec = GenSpec
    { wallets    :: !Integer
    -- ^ How many wallets to create
    , walletSpec :: WalletSpec
    -- ^ The specification for each wallet.
    } deriving (Show, Eq, Generic)

instance FromJSON GenSpec
instance ToJSON GenSpec

data WalletSpec = WalletSpec
    { accounts      :: !Integer
    -- ^ How many accounts to generate
    , accountSpec  :: AccountSpec
    -- ^ How specification for each account.
    , fakeUtxo      :: FakeUtxoSpec
    -- ^ Configuration for the generation of the fake UTxO.
    } deriving (Show, Eq, Generic)

instance FromJSON WalletSpec
instance ToJSON WalletSpec

data AccountSpec = AccountSpec
    { addresses :: !Integer
    -- ^ How many addresses to generate.
    } deriving (Show, Eq, Generic)

instance FromJSON AccountSpec
instance ToJSON AccountSpec

-- TODO(ks): The question here is whether we need to support some other
-- strategies for distributing money, like maybe using a fixed amount
-- of `toAddress` to cap the distribution - we have examples where we
-- have around 80 000 addresses which is a lot and can be intensive?
-- For now, KISS.
data FakeUtxoSpec = FakeUtxoSpec
    { amount       :: !Integer
    -- ^ How much ADA do we want to distribute in a single address.
    , distribution :: CoinDistributionSpec
    -- ^ Choose a @CoinDistributionSpec@.
    } deriving (Show, Eq, Generic)

instance FromJSON FakeUtxoSpec
instance ToJSON FakeUtxoSpec

data CoinDistributionSpec
    = NoDistribution
    -- ^ Do not distribute the coins.
    | RangeDistribution { range :: !Integer }
    -- ^ Distributes to only XX addresses.
    -- ^ TODO(adn): For now we KISS, later we can add more type constructors
    deriving (Show, Eq, Generic)

instance FromJSON CoinDistributionSpec
instance ToJSON CoinDistributionSpec

{-
λ> encode NoDistribution
"{\"tag\":\"NoDistribution\"}"
λ> encode $ RangeDistribution { range=100 }
"{\"tag\":\"RangeDistribution\",\"range\":100}"
-}

--
-- Functions
--

-- | Load the 'GenSpec' from an input file.
loadGenSpec :: FilePath -> IO GenSpec
loadGenSpec fpath = do
    fileContent <- B.readFile fpath
    either exception pure (eitherDecodeStrict fileContent)
  where
    -- TODO(ks): MonadThrow maybe?
    exception = error "Invalid JSON configuration file format!"

-- | Run an action and report the time it took.
timed :: MonadIO m => m a -> m a
timed action = do
    before  <- liftIO getCurrentTime
    res     <- action
    after   <- liftIO getCurrentTime
    -- NOTE: Mind the `fromEnum` overflow.
    let diff = fromEnum $ after `diffUTCTime` before

    let printString :: String
        printString = printf "Action took %f seconds." (fromIntegral diff / (1000000000000 :: Double))

    liftIO $ putStrLn printString
    return res


fakeSync :: UberMonad ()
fakeSync = do
    say "Faking StateLock syncing..."
    tip <- getTip
    (StateLock mvar _) <- view (lensOf @StateLock)
    () <$ tryPutMVar mvar tip

-- | The main entry point.
generateWalletDB :: CLI -> GenSpec -> UberMonad ()
generateWalletDB CLI{..} spec@GenSpec{..} = do
    fakeSync
    -- If `addTo` is not mempty, skip the generation
    -- but append the requested addresses to the input
    -- CAccountId.

    -- TODO(ks): Simplify this?
    let fakeUtxoSpec = fakeUtxo walletSpec
    case addTo of
        Just accId -> case (distribution fakeUtxoSpec) of
            NoDistribution -> addAddressesTo accId spec
            _              -> generateFakeUtxo fakeUtxoSpec accId
        Nothing -> do
            say $ printf "Generating %d wallets..." wallets
            wallets' <- timed (forM [1..wallets] genWallet)
            forM_ (zip [1..] wallets') (genAccounts spec)
    say $ green "OK."

generateFakeUtxo :: FakeUtxoSpec -> AccountId -> UberMonad ()
generateFakeUtxo FakeUtxoSpec{..} aId = do
    let fromAddr = distrToNumAddr distribution
    -- First let's generate the initial addesses where we will fake money from.
    genCAddresses <- timed $ forM [1..fromAddr] (const $ genAddress aId)

    let generatedAddresses = rights $ map unwrapCAddress genCAddresses

    let coinAmount :: Coin
        coinAmount = mkCoin $ fromIntegral amount

    let txsOut :: [TxOutAux]
        txsOut = map (\address -> TxOutAux $ TxOut address coinAmount) generatedAddresses

    utxo           <- getWalletUtxo

    txInTxOutTuple <- liftIO $ sequence [ (,) <$> genTxIn <*> pure txOut | txOut <- txsOut ]
    let newUtxo    = utxo `union` fromList txInTxOutTuple

    setWalletUtxo newUtxo

    -- Update state
    let mapModifier = utxoToModifier newUtxo
    updateWalletBalancesAndUtxo mapModifier
  where
    genTxIn :: IO TxIn
    genTxIn = generate $ TxInUtxo <$> arbitrary <*> arbitrary

    unwrapCAddress :: CAddress -> Either Text Address
    unwrapCAddress = decodeCType . cadId

    distrToNumAddr :: CoinDistributionSpec -> Integer
    distrToNumAddr NoDistribution             = 0
    distrToNumAddr (RangeDistribution range)  = range


addAddressesTo :: AccountId -> GenSpec -> UberMonad ()
addAddressesTo cid spec = genAddresses spec cid

genAccounts :: GenSpec -> (Int, CWallet) -> UberMonad ()
genAccounts spec@(walletSpec -> wspec) (idx, wallet) = do
    let accs = accounts wspec
    say $ printf "Generating %d accounts for Wallet %d..." accs idx
    cAccounts <- timed (forM [1..accs] (genAccount wallet))
    let cids = map toAccountId cAccounts
    forM_ cids (genAddresses spec)

toAccountId :: CAccount -> AccountId
toAccountId CAccount{..} = either (error . toS) id (decodeCType caId)

genAddresses :: GenSpec -> AccountId -> UberMonad ()
genAddresses (accountSpec . walletSpec -> aspec) cid = do
    let addrs = addresses aspec
    say $ printf "Generating %d addresses for Account %s..." addrs (renderAccountId cid)
    timed (forM_ [1..addrs] (const $ genAddress cid))

-- | Creates a new 'CWallet'.
genWallet :: Integer -> UberMonad CWallet
genWallet walletNum = do
    entropy   <- randomNumber walletNum
    mnemonic  <- newRandomMnemonic (toEntropy entropy)
    newWallet mempty (walletInit mnemonic)
  where
    walletInit :: BackupPhrase -> CWalletInit
    walletInit backupPhrase = CWalletInit {
      cwInitMeta      = CWalletMeta
          { cwName      = "Wallet #" <> show walletNum
          , cwAssurance = CWANormal
          , cwUnit      = 0
        }
      , cwBackupPhrase  = backupPhrase
      }

-- | Generates some 'Entropy' from an 'Integer'. Due to the fact
-- Haskoin accepts only multiple of 4 bytes, we pad the input with
-- zeros. It leads to quite crappy 12-words mnemonic, but as long as
-- they don't clash with each other, we are happy.
toEntropy :: Integer -> Entropy
toEntropy x =
    let packs = B.unpack $ show x
        len   = length packs
    in B.pack $ case len < 16 of
      True  -> packs <> replicate (16 - len) 0x0
      False -> take (len - (len `mod` 16)) packs

-- | Generates a new 'BackupPhrase' piggybacking on @Haskoin@ for
-- the BIP-39 words list.
newRandomMnemonic :: MonadIO m => Entropy -> m BackupPhrase
newRandomMnemonic entropy = case toMnemonic entropy of
    Left e  -> error (fromString $ "Error: " <> e)
    Right x -> pure (mkBackupPhrase12 (words $ toS x))

-- | Creates a new 'CAccount'.
genAccount :: CWallet -> Integer -> UberMonad CAccount
genAccount CWallet{..} accountNum = do
    newAccountIncludeUnready True RandomSeed mempty accountInit
  where
    accountInit :: CAccountInit
    accountInit = CAccountInit {
      caInitMeta = CAccountMeta {
          caName = "Account Number #" <> show accountNum
          }
      , caInitWId  = cwId
      }

-- | Creates a new 'CAddress'.
genAddress :: AccountId -> UberMonad CAddress
genAddress cid = do
    let (walletId, addrNum) = (aiWId cid, aiIndex cid)
    newAddress RandomSeed mempty (AccountId walletId addrNum)
