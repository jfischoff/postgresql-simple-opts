{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}
{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module Database.PostgreSQL.Simple.Options where
import Database.PostgreSQL.Simple
import Options.Applicative
import Text.Read
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import GHC.Generics
import Options.Generic
import Data.Typeable
import Data.String
import Data.Monoid
import Data.Either.Validation
import Data.Default

-- | An optional version of 'ConnectInfo'. This includes an instance of
-- | 'ParseRecord' which provides the optparse-applicative Parser.
data PartialConnectInfo = PartialConnectInfo
  { host     :: Last String
  , port     :: Last Int
  , user     :: Last String
  , password :: Last String
  , database :: Last String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance ParseRecord PartialConnectInfo

instance Monoid PartialConnectInfo where
  mempty = PartialConnectInfo (Last Nothing) (Last Nothing)
                              (Last Nothing) (Last Nothing)
                              (Last Nothing)
  mappend x y = PartialConnectInfo
    { host     = host     x <> host     y
    , port     = port     x <> port     y
    , user     = user     x <> user     y
    , password = password x <> password y
    , database = database x <> database y
    }

newtype ConnectString = ConnectString
  { connectString :: ByteString
  } deriving ( Show, Eq, Read, Ord, Generic, Typeable, IsString )

unSingleQuote :: String -> Maybe String
unSingleQuote (x : xs@(_ : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise                    = Nothing
unSingleQuote _                  = Nothing

parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> Just x

instance ParseRecord ConnectString where
  parseRecord =  fmap (ConnectString . BSC.pack)
              $  option ( eitherReader
                        $ maybe (Left "Impossible!") Right
                        . parseString
                        )
                        (long "connectString")

data PartialOptions
  = POConnectString      ConnectString
  | POPartialConnectInfo PartialConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)

instance Monoid PartialOptions where
    mempty = POPartialConnectInfo mempty
    mappend a b = case (a, b) of
        (POConnectString x, _) -> POConnectString x
        (POPartialConnectInfo x, POPartialConnectInfo y) ->
            POPartialConnectInfo $ x <> y
        (POPartialConnectInfo _, POConnectString x) -> POConnectString x

instance ParseRecord PartialOptions where
  parseRecord
    =  fmap POConnectString      parseRecord
   <|> fmap POPartialConnectInfo parseRecord

-- | The main parser to reuse.
parser :: Parser PartialOptions
parser = parseRecord

data Options
  = OConnectString ByteString
  | OConnectInfo   ConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)

mkLast :: a -> Last a
mkLast = Last . Just

-- | The 'PartialConnectInfo' version of 'defaultConnectInfo'
instance Default PartialConnectInfo where
    def = PartialConnectInfo
      { host     = mkLast $                connectHost     defaultConnectInfo
      , port     = mkLast $ fromIntegral $ connectPort     defaultConnectInfo
      , user     = mkLast $                connectUser     defaultConnectInfo
      , password = mkLast $                connectPassword defaultConnectInfo
      , database = mkLast $                connectDatabase defaultConnectInfo
      }

instance Default PartialOptions where
    def = POPartialConnectInfo def

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Data.Either.Validation.Failure
        ["Missing " ++ optionName ++ " option"]

completeConnectInfo :: PartialConnectInfo -> Either [String] ConnectInfo
completeConnectInfo PartialConnectInfo {..} = validationToEither $ do
  ConnectInfo <$> getOption "host"     host
              <*> (fromIntegral <$> getOption "port" port)
              <*> getOption "user"     user
              <*> getOption "password" password
              <*> getOption "database" database

-- | mappend with 'defaultPartialConnectInfo' if necessary to create all
--   options
completeOptions :: PartialOptions -> Either [String] Options
completeOptions = \case
  POConnectString   (ConnectString x) -> Right $ OConnectString x
  POPartialConnectInfo x              -> OConnectInfo <$> completeConnectInfo x

-- | Useful for testing or if only Options are needed.
completeParser :: Parser Options
completeParser =
    fmap (either (error . unlines) id . completeOptions . mappend def) parseRecord

-- | Create a connection with an 'Option'
run :: Options -> IO Connection
run = \case
  OConnectString connString -> connectPostgreSQL connString
  OConnectInfo   connInfo   -> connect           connInfo
