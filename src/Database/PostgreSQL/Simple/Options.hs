{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}
{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, GADTs, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Database.PostgreSQL.Simple.Options where
import Database.PostgreSQL.Simple
import Options.Applicative
import Text.Read
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import GHC.Generics
import Options.Generic
import Data.Typeable
import Data.Monoid
import Data.Either.Validation
import Data.Default
import URI.ByteString as URI
import Control.Monad
import Data.List.Split
import Data.List (intercalate)
import Generics.Deriving.Monoid
import Data.Char
import Data.Maybe

data Options = Options
  { oHost                    :: Maybe String
  , oHostaddr                :: Maybe String
  , oPort                    :: Int
  , oUser                    :: String
  , oPassword                :: String
  , oDbname                  :: String
  , oConnectTimeout          :: Maybe Int
  , oClientEncoding          :: Maybe String
  , oOptions                 :: Maybe String
  , oFallbackApplicationName :: Maybe String
  , oKeepalives              :: Maybe Int
  , oKeepalivesIdle          :: Maybe Int
  , oKeepalivesCount         :: Maybe Int
  , oSslmode                 :: Maybe String
  , oRequiressl              :: Maybe Int
  , oSslcompression          :: Maybe Int
  , oSslcert                 :: Maybe String
  , oSslkey                  :: Maybe String
  , oSslrootcert             :: Maybe String
  , oRequirepeer             :: Maybe String
  , oKrbsrvname              :: Maybe String
  , oGsslib                  :: Maybe String
  , oService                 :: Maybe String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)
-- | An optional version of 'Options'. This includes an instance of
-- | 'ParseRecord' which provides the optparse-applicative Parser.
data PartialOptions = PartialOptions
  { host                    :: Last String
  , hostaddr                :: Last String
  , port                    :: Last Int
  , user                    :: Last String
  , password                :: Last String
  , dbname                  :: Last String
  , connectTimeout          :: Last Int
  , clientEncoding          :: Last String
  , options                 :: Last String
  , fallbackApplicationName :: Last String
  , keepalives              :: Last Int
  , keepalivesIdle          :: Last Int
  , keepalivesCount         :: Last Int
  , sslmode                 :: Last String
  , requiressl              :: Last Int
  , sslcompression          :: Last Int
  , sslcert                 :: Last String
  , sslkey                  :: Last String
  , sslrootcert             :: Last String
  , requirepeer             :: Last String
  , krbsrvname              :: Last String
  , gsslib                  :: Last String
  , service                 :: Last String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance ParseRecord PartialOptions where
  parseRecord = (option (eitherReader parseConnectionString) (long "connectString"))
    <|> parseRecordWithModifiers defaultModifiers

instance Monoid PartialOptions where
  mempty = gmemptydefault
  mappend = gmappenddefault

-- Copied from Options.Generic source code
underscoreModifiers :: Modifiers
underscoreModifiers = Modifiers lispCase lispCase
  where
    lispCase = dropWhile (== '_') . (>>= lower) . dropWhile (== '_')
    lower c | isUpper c = ['_', toLower c]
            | otherwise = [c]


unSingleQuote :: String -> Maybe String
unSingleQuote (x : xs@(_ : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise                    = Nothing
unSingleQuote _                  = Nothing

parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> Just x

mkLast :: a -> Last a
mkLast = Last . Just

-- | The 'PartialOptions' version of 'defaultOptions'
instance Default PartialOptions where
    def = mempty
      { host     = mkLast $                connectHost     defaultConnectInfo
      , port     = mkLast $ fromIntegral $ connectPort     defaultConnectInfo
      , user     = mkLast $                connectUser     defaultConnectInfo
      , password = mkLast $                connectPassword defaultConnectInfo
      , dbname = mkLast $                  connectDatabase defaultConnectInfo
      }

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Data.Either.Validation.Failure
        ["Missing " ++ optionName ++ " option"]

getLast' :: Applicative f => Last a -> f (Maybe a)
getLast' = pure . getLast

completeOptions :: PartialOptions -> Either [String] Options
completeOptions PartialOptions {..} = validationToEither $ do
  Options <$> getLast' host
          <*> getLast' hostaddr
          <*> (fromIntegral <$> getOption "port" port)
          <*> getOption "user"     user
          <*> getOption "password" password
          <*> getOption "dbname" dbname
          <*> getLast' connectTimeout
          <*> getLast' clientEncoding
          <*> getLast' options
          <*> getLast' fallbackApplicationName
          <*> getLast' keepalives
          <*> getLast' keepalivesIdle
          <*> getLast' keepalivesCount
          <*> getLast' sslmode
          <*> getLast' requiressl
          <*> getLast' sslcompression
          <*> getLast' sslcert
          <*> getLast' sslkey
          <*> getLast' sslrootcert
          <*> getLast' requirepeer
          <*> getLast' krbsrvname
          <*> getLast' gsslib
          <*> getLast' service

maybeToPair :: Show a => String -> Maybe a -> [(String, String)]
maybeToPair k mv = (\v -> (k, show v)) <$> maybeToList mv

toConnectionString :: Options -> ByteString
toConnectionString Options {..} = BSC.pack $ unwords $ map (\(k, v) -> k <> "=" <> v)
  $  maybeToPair "host" oHost
  <> maybeToPair "hostaddr" oHostaddr
  <> [ ("port", show oPort)
     , ("user", oUser)
     , ("password", oPassword)
     , ("dbname", oDbname)
     ]
  <> maybeToPair "connect_timeout" oConnectTimeout
  <> maybeToPair "client_encoding" oClientEncoding
  <> maybeToPair "options" oOptions
  <> maybeToPair "fallback_applicationName" oFallbackApplicationName
  <> maybeToPair "keepalives" oKeepalives
  <> maybeToPair "keepalives_idle" oKeepalivesIdle
  <> maybeToPair "keepalives_count" oKeepalivesCount
  <> maybeToPair "sslmode" oSslmode
  <> maybeToPair "requiressl" oRequiressl
  <> maybeToPair "sslcompression" oSslcompression
  <> maybeToPair "sslcert" oSslcert
  <> maybeToPair "sslkey" oSslkey
  <> maybeToPair "sslrootcert" oSslrootcert
  <> maybeToPair "requirepeer" oRequirepeer
  <> maybeToPair "krbsrvname" oKrbsrvname
  <> maybeToPair "gsslib" oGsslib
  <> maybeToPair "service" oService


-- | Useful for testing or if only Options are needed.
completeParser :: Parser Options
completeParser =
    fmap (either (error . unlines) id . completeOptions . mappend def) parseRecord

-- | Create a connection with an 'Option'
run :: Options -> IO Connection
run = connectPostgreSQL . toConnectionString

userInfoToPartialOptions :: UserInfo -> PartialOptions
userInfoToPartialOptions UserInfo {..} = mempty { user = return $ BSC.unpack uiUsername } <> if BS.null uiPassword
  then mempty
  else mempty { password = return $ BSC.unpack uiPassword }

autorityToPartialOptions :: Authority -> PartialOptions
autorityToPartialOptions Authority {..} = maybe mempty userInfoToPartialOptions authorityUserInfo <>
  mempty { host = return $ BSC.unpack $ hostBS authorityHost } <>
  maybe mempty (\p -> mempty { port = return $ portNumber p }) authorityPort

pathToPartialOptions :: ByteString -> PartialOptions
pathToPartialOptions path = case drop 1 $ BSC.unpack path of
  "" -> mempty
  x  -> mempty {dbname = return x }

parseInt :: String -> String -> Either String Int
parseInt msg v = maybe (Left (msg <> " value of: " <> v <> " is not a number")) Right $
      readMaybe v

keywordToPartialOptions :: String -> String -> Either String PartialOptions
keywordToPartialOptions k v = case k of
  "host" -> return $ mempty { host = return $ v }
  "hostaddress" -> return $ mempty { hostaddr = return $ v }
  "port" -> do
    portValue <- parseInt "port" v
    return $ mempty { port = return portValue }
  "user" -> return $ mempty { user = return v }
  "password" -> return $ mempty { password = return v }
  "dbname" -> return $ mempty { dbname = return v}
  "connect_timeout" -> do
    x <- parseInt "connect_timeout" v
    return $ mempty { connectTimeout = return x }
  "client_encoding" -> return $ mempty { clientEncoding = return v }
  "options" -> return $ mempty { options = return v }
  "fallback_applicationName" -> return $ mempty { fallbackApplicationName = return v }
  "keepalives" -> do
    x <- parseInt "keepalives" v
    return $ mempty { keepalives = return x }
  "keepalives_idle" -> do
    x <- parseInt "keepalives_idle" v
    return $ mempty { keepalivesIdle = return x }
  "keepalives_count" -> do
    x <- parseInt "keepalives_count" v
    return $ mempty { keepalivesCount = return x }
  "sslmode" -> return $ mempty { sslmode = return v }
  "requiressl" -> do
    x <- parseInt "requiressl" v
    return $ mempty { requiressl = return x }
  "sslcompression" -> do
    x <- parseInt "sslcompression" v
    return $ mempty { sslcompression = return x }
  "sslcert" -> return $ mempty { sslcert = return v }
  "sslkey" -> return $ mempty { sslkey = return v }
  "sslrootcert" -> return $ mempty { sslrootcert = return v }
  "requirepeer" -> return $ mempty { requirepeer = return v }
  "krbsrvname" -> return $ mempty { krbsrvname = return v }
  "gsslib" -> return $ mempty { gsslib = return v }
  "service" -> return $ mempty { service = return v }

  x -> Left $ "Unrecongnized option: " ++ show x

queryToPartialOptions :: URI.Query -> Either String PartialOptions
queryToPartialOptions Query {..} = foldM (\acc (k, v) -> fmap (mappend acc) $ keywordToPartialOptions (BSC.unpack k) $ BSC.unpack v) mempty queryPairs

uriToOptions :: URIRef Absolute -> Either String PartialOptions
uriToOptions URI {..} = case schemeBS uriScheme of
  "postgresql" -> do
    queryParts <- queryToPartialOptions uriQuery
    return $ maybe mempty autorityToPartialOptions uriAuthority <>
      pathToPartialOptions uriPath <> queryParts

  x -> Left $ "Wrong protocol. Expected \"postgresql\" but got: " ++ show x

parseURIStr :: String -> Either String (URIRef Absolute)
parseURIStr = left show . parseURI strictURIParserOptions . BSC.pack where
  left f = \case
    Left x -> Left $ f x
    Right x -> Right x

parseKeywords :: String -> Either String PartialOptions
parseKeywords [] = Left "Failed to parse keywords"
parseKeywords x = fmap mconcat . mapM (uncurry keywordToPartialOptions <=< toTuple . splitOn "=") $ words x where
  toTuple [k, v] = return (k, v)
  toTuple xs = Left $ "invalid opts:" ++ show (intercalate "=" xs)

parseConnectionString :: String -> Either String PartialOptions
parseConnectionString url = do
  url' <- maybe (Left "failed to parse as string") Right $ parseString url
  parseKeywords url' <|> (uriToOptions =<< parseURIStr url')

toArgs :: ConnectInfo -> [String]
toArgs ConnectInfo {..} =
  [ "--dbname=" <> connectDatabase
  , "--username=" <> connectUser
  , "--port=" <> show connectPort
  , "--password=" <> connectPassword
  , "--host=" <> connectHost
  ]
