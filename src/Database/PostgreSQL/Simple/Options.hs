{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}
{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, GADTs, OverloadedStrings #-}
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

type Options = ConnectInfo

-- | An optional version of 'Options'. This includes an instance of
-- | 'ParseRecord' which provides the optparse-applicative Parser.
data PartialOptions = PartialOptions
  { host     :: Last String
  , port     :: Last Int
  , user     :: Last String
  , password :: Last String
  , database :: Last String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance ParseRecord PartialOptions where
  parseRecord = (option (eitherReader parseConnectionString) (long "connectString"))
    <|> parseRecordWithModifiers defaultModifiers

instance Monoid PartialOptions where
  mempty = PartialOptions (Last Nothing) (Last Nothing)
                              (Last Nothing) (Last Nothing)
                              (Last Nothing)
  mappend x y = PartialOptions
    { host     = host     x <> host     y
    , port     = port     x <> port     y
    , user     = user     x <> user     y
    , password = password x <> password y
    , database = database x <> database y
    }

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
    def = PartialOptions
      { host     = mkLast $                connectHost     defaultConnectInfo
      , port     = mkLast $ fromIntegral $ connectPort     defaultConnectInfo
      , user     = mkLast $                connectUser     defaultConnectInfo
      , password = mkLast $                connectPassword defaultConnectInfo
      , database = mkLast $                connectDatabase defaultConnectInfo
      }

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Data.Either.Validation.Failure
        ["Missing " ++ optionName ++ " option"]

completeOptions :: PartialOptions -> Either [String] Options
completeOptions PartialOptions {..} = validationToEither $ do
  ConnectInfo <$> getOption "host"     host
              <*> (fromIntegral <$> getOption "port" port)
              <*> getOption "user"     user
              <*> getOption "password" password
              <*> getOption "database" database


-- | Useful for testing or if only Options are needed.
completeParser :: Parser Options
completeParser =
    fmap (either (error . unlines) id . completeOptions . mappend def) parseRecord

-- | Create a connection with an 'Option'
run :: Options -> IO Connection
run = connect

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
  x  -> mempty {database = return x }

keywordToPartialOptions :: String -> String -> Either String PartialOptions
keywordToPartialOptions k v = case k of
  "host" -> return $ mempty { host = return $ v }
  "port" -> do
    portValue <- maybe (Left ("port value of: " <> v <> " is not a number")) Right $
      readMaybe v

    return $ mempty { port = return portValue }
  "user" -> return $ mempty { user = return v }
  "password" -> return $ mempty { password = return v }
  "dbname" -> return $ mempty { database = return v}
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
