{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}
{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE CPP, GADTs, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Database.PostgreSQL.Simple.PartialOptions.Internal where

import Control.Monad ((<=<), foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isUpper, toLower)
import Data.Default (Default(..))
import qualified Data.Either.Validation as DEV
import Data.Either.Validation (Validation(..))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, connectPostgreSQL, defaultConnectInfo)
import qualified Database.PostgreSQL.Simple.Options as O
import Database.PostgreSQL.Simple.Options(Options)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (Last(..), gmappenddefault, gmemptydefault)
import Options.Applicative (Parser, (<|>), eitherReader, long, option)
import Options.Generic (Modifiers(..), ParseRecord(..), defaultModifiers, parseRecordWithModifiers)
import Text.Read (readMaybe)
import URI.ByteString as URI
import System.Envy hiding (Parser)

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

instance FromEnv PartialOptions where
  fromEnv _
      = PartialOptions
    <$> env "PGHOST"
    <*> env "PGHOSTADDR"
    <*> env "PGPORT"
    <*> env "PGUSER"
    <*> env "PGPASSWORD"
    <*> env "PGDATABASE"
    <*> env "PGCONNECT_TIMEOUT"
    <*> env "PGCLIENTENCODING"
    <*> env "PGOPTIONS"
    <*> env "PGAPPNAME"
    <*> env "PGKEEPALIVES"
    <*> env "PGKEEPALIVESIDLE"
    <*> env "PGKEEPALIVESCOUNT"
    <*> env "PGSSLMODE"
    <*> env "PGREQUIRESSL"
    <*> env "PGSSLCOMPRESSION"
    <*> env "PGSSLCERT"
    <*> env "PGSSLKEY"
    <*> env "PGSSLROOTCERT"
    <*> env "PGREQUIREPEER"
    <*> env "PGKRBSRVNAME"
    <*> env "PGGSSLIB"
    <*> env "PGSERVICE"

instance ToEnv PartialOptions where
  toEnv PartialOptions {..} = makeEnv
    [ "PGHOST"            .= host
    , "PGHOSTADDR"        .= hostaddr
    , "PGPORT"            .= port
    , "PGUSER"            .= user
    , "PGPASSWORD"        .= password
    , "PGDATABASE"        .= dbname
    , "PGCONNECT_TIMEOUT" .= connectTimeout
    , "PGCLIENTENCODING"  .= clientEncoding
    , "PGOPTIONS"         .= options
    , "PGAPPNAME"         .= fallbackApplicationName
    , "PGKEEPALIVES"      .= keepalives
    , "PGKEEPALIVESIDLE"  .= keepalivesIdle
    , "PGKEEPALIVESCOUNT" .= keepalivesCount
    , "PGSSLMODE"         .= sslmode
    , "PGREQUIRESSL"      .= requiressl
    , "PGSSLCOMPRESSION"  .= sslcompression
    , "PGSSLCERT"         .= sslcert
    , "PGSSLKEY"          .= sslkey
    , "PGSSLROOTCERT"     .= sslrootcert
    , "PGREQUIREPEER"     .= requirepeer
    , "PGKRBSRVNAME"      .= krbsrvname
    , "PGGSSLIB"          .= gsslib
    , "PGSERVICE"         .= service
    ]

instance DefConfig PartialOptions where
  defConfig = mempty

instance ParseRecord PartialOptions where
  parseRecord = option (eitherReader parseConnectionString) (long "connectString")
    <|> parseRecordWithModifiers defaultModifiers

instance Semigroup PartialOptions where
  (<>) = gmappenddefault

instance Monoid PartialOptions where
  mempty = gmemptydefault
  mappend = (<>)

-- Copied from Options.Generic source code
underscoreModifiers :: Modifiers
underscoreModifiers = Modifiers lispCase lispCase (const Nothing)
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
      , dbname   = mkLast $                connectDatabase defaultConnectInfo
      }

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> DEV.Failure ["Missing " ++ optionName ++ " option"]

getLast' :: Applicative f => Last a -> f (Maybe a)
getLast' = pure . getLast

-- This is pointless.
completeOptions :: PartialOptions -> Either [String] Options
completeOptions PartialOptions {..} = pure $
  O.Options host
            hostaddr
            (fromIntegral <$> port)
            user
            password
            dbname
            connectTimeout
            clientEncoding
            options
            fallbackApplicationName
            keepalives
            keepalivesIdle
            keepalivesCount
            sslmode
            requiressl
            sslcompression
            sslcert
            sslkey
            sslrootcert
            requirepeer
            krbsrvname
            gsslib
            service


-- | Useful for testing or if only Options are needed.
completeParser :: Parser Options
completeParser =
    fmap (either (error . unlines) id . completeOptions . mappend def) parseRecord

-- | Create a connection with an 'Option'
run :: Options -> IO Connection
run = connectPostgreSQL . O.toConnectionString

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
  "host" -> return $ mempty { host = return v }
  "hostaddress" -> return $ mempty { hostaddr = return v }
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
