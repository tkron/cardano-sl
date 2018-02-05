{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( getWallet
       , getWallets
       , getAccount
       , getAccounts

       , createWalletSafe
       , newAccount
       , newAccountIncludeUnready
       , newAddress
       , newAddress_

       , deleteWallet
       , deleteAccount

       , updateWallet
       , updateAccount
       , changeWalletPassphrase
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (findIndex)
import qualified Data.Set                   as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, (%))

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Core                   (Address, Coin, mkCoin, sumCoins,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (PassPhrase, changeEncPassphrase,
                                             checkPassMatches, emptyPassphrase)
import           Pos.Txp                    (applyUtxoModToAddrCoinMap)
import           Pos.Util                   (maybeThrow)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.KeyStorage      (addSecretKey, deleteSecretKey,
                                             getSecretKeysPlain)
import           Pos.Wallet.Web.Account     (AddrGenSeed, findKey, genUniqueAccountId,
                                             genUniqueAddress, getAddrIdx, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..),
                                             CAccountInit (..), CAccountMeta (..),
                                             CAddress (..), CId, CWAddressMeta (..),
                                             CWallet (..), CWalletMeta (..), Wal,
                                             addrMetaToAccount, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode, convertCIdTOAddr,
                                             convertCIdTOAddrs)
import           Pos.Wallet.Web.State       (AddressInfo (..),
                                             AddressLookupMode (Deleted, Ever, Existing),
                                             CustomAddressType (ChangeAddr, UsedAddr),
                                             WalletSnapshot, addWAddress, askWalletDB,
                                             askWalletSnapshot, createAccount,
                                             createWallet, doesAccountExist,
                                             getAccountIds, getWalletAddresses,
                                             getWalletBalancesAndUtxo,
                                             getWalletMetaIncludeUnready, getWalletPassLU,
                                             getWalletSnapshot, isCustomAddress,
                                             removeAccount, removeHistoryCache,
                                             removeTxMetas, removeWallet, setAccountMeta,
                                             setWalletMeta, setWalletPassLU)
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), CachedCAccModifier,
                                             immModifier, sortedInsertions,
                                             txMempoolToModifier)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getAccountMetaOrThrow, getWalletAccountIds,
                                             getWalletAddrMetas)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getAccountMod
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier
    -> AccountId
    -> m CAccount
getAccountMod ws accMod accId = do
    dbAddrs    <- map adiCWAddressMeta . sortOn adiSortingKey <$> getAccountAddrsOrThrow ws Existing accId
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
    allAddrs <- mapM (getWAddress ws accMod) allAddrIds
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM (decodeCTypeOrFail . cadAmount) allAddrs
    meta <- getAccountMetaOrThrow ws accId
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    gatherAddresses addrModifier dbAddrs = do
        let memAddrs = sortedInsertions addrModifier
            dbAddrsSet = S.fromList dbAddrs
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            unknownMemAddrs = filter (`S.notMember` dbAddrsSet) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getAccount :: MonadWalletWebMode m => AccountId -> m CAccount
getAccount accId = do
    ws <- askWalletSnapshot
    accMod <- txMempoolToModifier ws =<< findKey accId
    getAccountMod ws accMod accId

getAccountsIncludeUnready
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready ws includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr ->
      void $ maybeThrow (noWallet cAddr) $
        getWalletMetaIncludeUnready ws includeUnready cAddr
    let accIds = maybe (getAccountIds ws) (getWalletAccountIds ws) mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) -> do
      accMod <- txMempoolToModifier ws =<< findKey wid
      mapM (getAccountMod ws accMod) walAccIds
  where
    noWallet cAddr = RequestError $
        -- TODO No WALLET with id ...
        -- dunno whether I can fix and not break compatible w/ daedalus
        sformat ("No account with id "%build%" found") cAddr

getAccounts
    :: MonadWalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    ws <- askWalletSnapshot
    getAccountsIncludeUnready ws False mCAddr

getWalletIncludeUnready :: MonadWalletWebMode m
                        => WalletSnapshot -> Bool -> CId Wal -> m CWallet
getWalletIncludeUnready ws includeUnready cWalId = do
    meta       <- maybeThrow noWallet $ getWalletMetaIncludeUnready ws includeUnready cWalId
    accounts   <- getAccountsIncludeUnready ws includeUnready (Just cWalId)
    let accountsNum = length accounts
    accMod     <- txMempoolToModifier ws =<< findKey cWalId
    balance    <- computeBalance accMod
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cWalId
    passLU     <- maybeThrow noWallet (getWalletPassLU ws cWalId)
    pure $ CWallet cWalId meta accountsNum balance hasPass passLU
  where
    computeBalance accMod = do
        let waddrIds = getWalletWAddrsWithMod ws Existing accMod cWalId
        addrIds <- convertCIdTOAddrs (map cwamId waddrIds)
        let coins = getBalancesWithMod ws accMod addrIds
        pure . mkCCoin . unsafeIntegerToCoin . sumCoins $ coins

    noWallet = RequestError $
        sformat ("No wallet with address "%build%" found") cWalId

getWallet :: MonadWalletWebMode m => CId Wal -> m CWallet
getWallet wid = do
    ws <- askWalletSnapshot
    getWalletIncludeUnready ws False wid

getWallets :: MonadWalletWebMode m => m [CWallet]
getWallets = do
    ws <- askWalletSnapshot
    mapM (getWalletIncludeUnready ws False) (getWalletAddresses ws)

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress_
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
newAddress_ ws addGenSeed passphrase accId = do
    -- check whether account exists
    let parentExists = doesAccountExist ws accId
    unless parentExists $ throwM noAccount

    -- XXX Transaction
    -- Make 'newAddress' generate a unique name internally
    cAccAddr <- genUniqueAddress ws addGenSeed passphrase accId
    db <- askWalletDB
    addWAddress db cAccAddr
    return cAccAddr
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

newAddress
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress ws addGenSeed passphrase accId = do
    cwAddrMeta <- newAddress_ ws addGenSeed passphrase accId
    accMod <- txMempoolToModifier ws =<< findKey accId
    getWAddress ws accMod cwAddrMeta

newAccountIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    -- TODO nclarke We read the mempool at this point to be consistent with the previous
    -- behaviour, but we may want to consider whether we should read it _after_ the
    -- account is created, since it's not used until we call 'getAccountMod'
    accMod <- txMempoolToModifier ws =<< findKey caInitWId
    -- check wallet exists
    _ <- getWalletIncludeUnready ws includeUnready caInitWId

    cAddr <- genUniqueAccountId ws addGenSeed caInitWId
    -- XXX Transaction
    () <- createAccount db cAddr caInitMeta
    ws' <- askWalletSnapshot
    _ <- newAddress ws' addGenSeed passphrase cAddr
    ws'' <- askWalletSnapshot

    -- Re-read DB after the update.
    getAccountMod ws'' accMod cAddr

newAccount
    :: MonadWalletWebMode m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount = newAccountIncludeUnready False

createWalletSafe
    :: MonadWalletWebMode m
    => CId Wal -> CWalletMeta -> Bool -> m CWallet
createWalletSafe cid wsMeta isReady = do
    -- Disallow duplicate wallets (including unready wallets)
    db <- askWalletDB
    ws <- getWalletSnapshot db
    let wSetExists = isJust $ getWalletMetaIncludeUnready ws True cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet db cid wsMeta isReady curTime
    -- Return the newly created wallet irrespective of whether it's ready yet
    ws' <- getWalletSnapshot db
    getWalletIncludeUnready ws' True cid


----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    db <- askWalletDB
    -- XXX Transaction
    accounts <- getAccounts (Just wid)
    mapM_ (removeAccount db <=< decodeCTypeOrFail . caId) accounts
    removeWallet db wid
    removeTxMetas db wid
    removeHistoryCache db wid
    deleteSecretKey . fromIntegral =<< getAddrIdx wid

deleteAccount :: MonadWalletWebMode m => AccountId -> m ()
deleteAccount accId = do
  db <- askWalletDB
  removeAccount db accId

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet :: MonadWalletWebMode m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    db <- askWalletDB
    setWalletMeta db wId wMeta
    getWallet wId

updateAccount :: MonadWalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    db <- askWalletDB
    setAccountMeta db accId wMeta
    getAccount accId

changeWalletPassphrase
    :: MonadWalletWebMode m
    => CId Wal -> PassPhrase -> PassPhrase -> m ()
changeWalletPassphrase wid oldPass newPass = do
    oldSK <- getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSK oldPass
        addSecretKey newSK
        db <- askWalletDB
        setWalletPassLU db wid =<< liftIO getPOSIXTime
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeysPlain
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBalanceWithMod :: WalletSnapshot -> CachedCAccModifier -> Address -> Coin
getBalanceWithMod ws accMod addr =
    let balancesAndUtxo = getWalletBalancesAndUtxo ws
    in  HM.lookupDefault (mkCoin 0) addr $
        flip applyUtxoModToAddrCoinMap balancesAndUtxo (camUtxo accMod)

getBalancesWithMod :: WalletSnapshot -> CachedCAccModifier -> [Address] -> [Coin]
getBalancesWithMod ws accMod addrs =
    let balancesAndUtxo = getWalletBalancesAndUtxo ws in
    let addrCoinsMap = applyUtxoModToAddrCoinMap (camUtxo accMod) balancesAndUtxo in
    let getBalance ad = HM.lookupDefault (mkCoin 0) ad addrCoinsMap in
    map getBalance addrs

getWAddressBalanceWithMod
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier
    -> CWAddressMeta
    -> m Coin
getWAddressBalanceWithMod ws accMod addr =
    getBalanceWithMod ws accMod
        <$> convertCIdTOAddr (cwamId addr)

-- BE CAREFUL: this function has complexity O(number of used and change addresses)
getWAddress
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier
    -> CWAddressMeta
    -> m CAddress
getWAddress ws cachedAccModifier cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalanceWithMod ws cachedAccModifier cAddr

    let getFlag customType accessMod = do
            let checkDB = isCustomAddress ws customType (cwamId cAddr)
            let checkMempool = elem aId . map (fst . fst) . toList $
                               MM.insertions $ accessMod cachedAccModifier
            return (checkDB || checkMempool)
    isUsed   <- getFlag UsedAddr camUsed
    isChange <- getFlag ChangeAddr camChange
    return $ CAddress aId (mkCCoin balance) isUsed isChange

getWalletWAddrsWithMod
    :: MonadWalletWebMode m
    => WalletSnapshot -> AddressLookupMode -> CachedCAccModifier -> CId Wal -> [CWAddressMeta]
getWalletWAddrsWithMod ws mode cAccMod wid =
    let dbAddresses = getWalletAddrMetas ws mode wid
        addrMapMod = MM.filterWithKey (\k _ -> cwamWId k == wid) $ immModifier $ camAddresses cAccMod
    in  case mode of
            Existing ->
                filter (not . flip HM.member (MM.toHashMap addrMapMod)) dbAddresses ++
                map fst (MM.insertions addrMapMod)
            Deleted  -> dbAddresses ++ MM.deletions addrMapMod
            Ever     -> dbAddresses ++ HM.keys (MM.toHashMap addrMapMod)
