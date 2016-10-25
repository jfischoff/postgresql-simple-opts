### Composable Command Line Parsing with `optparse-applicative`

There are many solutions for parsing command line arguments in Haskell. Personally I like [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative-0.12.1.0/), because, like the title suggests, you can compose parsers out of smaller pieces.

I have written command line parsers for the database connection info for [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/) many times and faced with the prospect of doing it again I opted to make a library. This way I could reuse it in web servers, db migrators, db job runners ... those are all the examples I could think of ... just trust me, it's worth it.

### Outline
- [The "Partial" Option Types](#partial)
- [The Composable Parser](#parser)
- [The Complete Option](#option)
- [Option "completion"](#completion)
- [The Option Parser](#option-parser)
- [The Runner](#runner)
- [The Tests](#tests)

### Standard Intro Statements to Ignore

```haskell
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
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
#endif
```

### <a name="partial"> The "Partial" Option Types

In general, options types are built from many optional fields. Additionally, multiple options sets can be combined (e.g. command line options, config file, environment vars, defaults, etc). The easiest way to handle this is to create a "partial" option family that can be monoidally composed and is "completed" with a default option value.

```haskell
data PartialConnectInfo = PartialConnectInfo
  { host     :: Last String
  , port     :: Last Int
  , user     :: Last String
  , password :: Last String
  , database :: Last String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)
```

We will utilize a boilerplate prevention library by Gaberiel Gonzales called [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic-1.1.3) to generate the parser for use from the records field names.

To create the parser we have to merely declare an instance of `ParseRecord`.

```haskell
instance ParseRecord PartialConnectInfo
```

Now we make `PartialConnectInfo` an instance of `Monoid` so we can combine multiple options together.

```haskell
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
```

As it so happens there are two ways to create a db connection with `postgresql-simple`: `ConnectInfo` and a `ByteString` connection string. We have a partial version of `ConnectInfo` but we need something for the connection string.

```haskell
newtype ConnectString = ConnectString
  { connectString :: ByteString
  } deriving ( Show, Eq, Read, Ord, Generic, Typeable, IsString )

```

I don't like the default option parsing for `String` in `optparse-applicative`. I want something that will escape double quotes, remove single quotes or just use the string unaltered. The function `parseString` does this.

```haskell
unSingleQuote :: String -> Maybe String
unSingleQuote (x : xs@(_ : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise                    = Nothing
unSingleQuote _                  = Nothing

parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> Just x
```

We use `parseString` to make a custom instance of `ParseRecord`.

```haskell
instance ParseRecord ConnectString where
  parseRecord =  fmap (ConnectString . BSC.pack)
              $  option ( eitherReader
                        $ maybe (Left "Impossible!") Right
                        . parseString
                        )
                        (long "connectString")
```
Thus, my `PartialOptions` type is either the `ConnectString` or the `PartialConnectInfo` type.

```haskell
data PartialOptions
  = POConnectString      ConnectString
  | POPartialConnectInfo PartialConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)
```

There is one wrinkle. `optparse-generic` treats sum types as "commands". This makes sense as a default, but it is not what we want. We want to choose one record or another based on the non-overlapping flags. This is easy enough to do by hand.

```haskell
instance ParseRecord PartialOptions where
  parseRecord
    =  fmap POConnectString      parseRecord
   <|> fmap POPartialConnectInfo parseRecord
```

### <a name="parser"> The Composable Parser

We can use `PartialOptions` as the type of a field in a larger options record defined elsewhere. When defining this more complicated parser, we reuse the work we did here by calling `parseRecord`. To make it even clearer we create an alias called `parser` so clients will know what to use.

```haskell
parser :: Parser PartialOptions
parser = parseRecord
```

### <a name="option"> The Complete Option

The connection option for `postgresql-simple` is either the record `ConnectInfo` or a connection string

```haskell
data Options
  = OConnectString ByteString
  | OConnectInfo   ConnectInfo
  deriving (Show, Eq, Read, Generic, Typeable)
```

### <a name="completion"> Option "completion"

`postgresql-simple` provides sensible defaults for `ConnectInfo` via `defaultConnectInfo`. We use these as the defaults when parsing. We create a `PartialConnectInfo` with these defaults.

```haskell
mkLast :: a -> Last a
mkLast = Last . Just

defaultPartialConnectInfo :: PartialConnectInfo
defaultPartialConnectInfo = PartialConnectInfo
  { host     = mkLast $                connectHost     defaultConnectInfo
  , port     = mkLast $ fromIntegral $ connectPort     defaultConnectInfo
  , user     = mkLast $                connectUser     defaultConnectInfo
  , password = mkLast $                connectPassword defaultConnectInfo
  , database = mkLast $                connectDatabase defaultConnectInfo
  }
```

We can now complete the `PartialConnectInfo` to get a `ConnectInfo`.

```haskell
completeConnectInfo :: PartialConnectInfo -> ConnectInfo
completeConnectInfo x = case defaultPartialConnectInfo <> x of
  PartialConnectInfo
    { host     = Last (Just connectHost    )
    , port     = Last (Just connectPortInt )
    , user     = Last (Just connectUser    )
    , password = Last (Just connectPassword)
    , database = Last (Just connectDatabase)
    } -> let connectPort = fromIntegral connectPortInt in ConnectInfo {..}
  _ -> error "Impossible! No options should be required!"
```

Completing a `PartialOptions` to get an `Options` follows straightforwardly ... if you've done this a bunch I suppose.

```haskell
completeOptions :: PartialOptions -> Options
completeOptions = \case
  POConnectString   (ConnectString x) -> OConnectString x
  POPartialConnectInfo x              -> OConnectInfo $ completeConnectInfo x
```

### <a name="option-parser"> The Option Parser

Parse a `PartialOptions` and then complete it. This is **not** composable but is convient for testing and if you only need a `Option` type

```haskell
completeParser :: Parser Options
completeParser = fmap completeOptions parseRecord
```

### <a name="runner"> The Runner

As a convenience, we export the primary use of parsing connection options ... making a connection.

```haskell
run :: Options -> IO Connection
run = \case
  OConnectString connString -> connectPostgreSQL connString
  OConnectInfo   connInfo   -> connect           connInfo
```

### <a name="tests"> The tests

Testing is pretty straightforward using `System.Environment.withArgs`. See the [Spec.hs](/test/Spec.hs) for examples of how to test the parsers.
