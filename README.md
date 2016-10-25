{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.PostgreSQL.Simple.Options where
import Database.PostgreSQL.Simple
import Options.Applicative
import Options.Applicative.Builder
import Text.Read
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import GHC.Generics
import Options.Generic
import Data.Word
import Data.Typeable
import Data.String

data PartialConnectInfo = PartialConnectInfo
  { host     :: Maybe String
  , port     :: Maybe Word16
  , user     :: Maybe String
  , password :: Maybe String
  , database :: Maybe String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance ParseField Word16

instance Monoid PartialConnectInfo where
  mempty = PartialConnectInfo Nothing Nothing Nothing Nothing Nothing
  mappend x y = PartialConnectInfo
    { host     = host     x <|> host     y
    , port     = port     x <|> port     y
    , user     = user     x <|> user     y
    , password = password x <|> password y
    , database = database x <|> database y
    }
  
defaultPartialConnectInfo :: PartialConnectInfo
defaultPartialConnectInfo = PartialConnectInfo
  { host     = Just $ connectHost     defaultConnectInfo 
  , port     = Just $ connectPort     defaultConnectInfo
  , user     = Just $ connectUser     defaultConnectInfo
  , password = Just $ connectPassword defaultConnectInfo 
  , database = Just $ connectDatabase defaultConnectInfo
  }

instance ParseRecord PartialConnectInfo

newtype ConnectionString = ConnectionString
  { connectionString :: ByteString
  } deriving ( Show, Eq, Read, Ord, Generic, Typeable, IsString )
  
unSingleQuote :: String -> Maybe String  
unSingleQuote (x : xs@(y : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise = Nothing

-- attempt to parse as a single quoted or double quoted string
parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> return x
  
instance ParseRecord ConnectionString where
  parseRecord =  fmap (ConnectionString . BSC.pack)
              $  option (eitherReader $ maybe (Left "Impossible!") Right .  parseString) 
                        (long "connectionString")
              
  
data PartialOptions 
  = POConnectionString   ConnectionString 
  | POPartialConnectInfo PartialConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)

data Options 
  = OConnectString ByteString 
  | OConnectInfo   ConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)
  
completeConnectInfo :: PartialConnectInfo -> ConnectInfo
completeConnectInfo x = case x <> defaultPartialConnectInfo of
  PartialConnectInfo 
    { host     = Just connectHost
    , port     = Just connectPort
    , user     = Just connectUser
    , password = Just connectPassword
    , database = Just connectDatabase
    } -> ConnectInfo {..}
  _ -> error "Impossible! No options should be required!"

completeOptions :: PartialOptions -> Options
completeOptions = \case
  POConnectionString   (ConnectionString x) -> OConnectString x
  POPartialConnectInfo x                    -> OConnectInfo $ completeConnectInfo x

run :: Options -> IO Connection
run = \case
  OConnectString str  -> connectPostgreSQL str
  OConnectInfo   info -> connect           info
  
parser :: Parser Options
parser 
  =  fmap completeOptions 
  $  fmap POConnectionString   parseRecord 
 <|> fmap POPartialConnectInfo parseRecord