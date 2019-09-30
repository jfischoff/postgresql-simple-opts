{-# LANGUAGE OverloadedStrings, CPP #-}
import Test.Hspec
import Database.PostgreSQL.Simple.PartialOptions
import Database.PostgreSQL.Simple.Options
import System.Environment
import Options.Applicative
import System.Exit
import Data.Default
import qualified System.Envy as E
import qualified Data.Map as Map

testParser :: IO Options
testParser = execParser $ info completeParser mempty

main :: IO ()
main = hspec $ do
  describe "Env Parser" $ do
    it "parses all options" $ do
      mapM_ (uncurry setEnv)
        [ ("PGHOST"           , "localhost")
        , ("PGHOSTADDR"       , "127.0.0.1")
        , ("PGPORT"           , "1234")
        , ("PGUSER"           , "postgres")
        , ("PGPASSWORD"       , "postgres")
        , ("PGDATABASE"       , "temp1")
        , ("PGCONNECT_TIMEOUT", "100")
        , ("PGCLIENTENCODING" , "something")
        , ("PGOPTIONS"        , "options")
        , ("PGAPPNAME"        , "da app")
        , ("PGKEEPALIVES"     , "1")
        , ("PGKEEPALIVESIDLE" , "2")
        , ("PGKEEPALIVESCOUNT", "3")
        , ("PGSSLMODE"        , "on")
        , ("PGREQUIRESSL"     , "1")
        , ("PGSSLCOMPRESSION" , "0")
        , ("PGSSLCERT"        , "ca.cert")
        , ("PGSSLKEY"         , "fda.key")
        , ("PGSSLROOTCERT"    , "root.cert")
        , ("PGREQUIREPEER"    , "yes")
        , ("PGKRBSRVNAME"     , "blah")
        , ("PGGSSLIB"         , "foo")
        , ("PGSERVICE"        , "bar")
        ]

      let expected = PartialOptions
            { host                    = pure "localhost"
            , hostaddr                = pure "127.0.0.1"
            , port                    = pure 1234
            , user                    = pure "postgres"
            , password                = pure "postgres"
            , dbname                  = pure "temp1"
            , connectTimeout          = pure 100
            , clientEncoding          = pure "something"
            , options                 = pure "options"
            , fallbackApplicationName = pure "da app"
            , keepalives              = pure 1
            , keepalivesIdle          = pure 2
            , keepalivesCount         = pure 3
            , sslmode                 = pure "on"
            , requiressl              = pure 1
            , sslcompression          = pure 0
            , sslcert                 = pure "ca.cert"
            , sslkey                  = pure "fda.key"
            , sslrootcert             = pure "root.cert"
            , requirepeer             = pure "yes"
            , krbsrvname              = pure "blah"
            , gsslib                  = pure "foo"
            , service                 = pure "bar"
            }

      E.decode `shouldReturn` Just expected

    it "adds to the environment all options" $ do
      let intial = PartialOptions
            { host                    = pure "localhost"
            , hostaddr                = pure "127.0.0.1"
            , port                    = pure 1234
            , user                    = pure "postgres"
            , password                = pure "postgres"
            , dbname                  = pure "temp1"
            , connectTimeout          = pure 100
            , clientEncoding          = pure "something"
            , options                 = pure "options"
            , fallbackApplicationName = pure "da app"
            , keepalives              = pure 1
            , keepalivesIdle          = pure 2
            , keepalivesCount         = pure 3
            , sslmode                 = pure "on"
            , requiressl              = pure 1
            , sslcompression          = pure 0
            , sslcert                 = pure "ca.cert"
            , sslkey                  = pure "fda.key"
            , sslrootcert             = pure "root.cert"
            , requirepeer             = pure "yes"
            , krbsrvname              = pure "blah"
            , gsslib                  = pure "foo"
            , service                 = pure "bar"
            }
      mapM_ (unsetEnv . fst) =<< getEnvironment

      E.setEnvironment' intial `shouldReturn` Right ()

      let expected = Map.fromList
            [ ("PGHOST"           , "localhost")
            , ("PGHOSTADDR"       , "127.0.0.1")
            , ("PGPORT"           , "1234")
            , ("PGUSER"           , "postgres")
            , ("PGPASSWORD"       , "postgres")
            , ("PGDATABASE"       , "temp1")
            , ("PGCONNECT_TIMEOUT", "100")
            , ("PGCLIENTENCODING" , "something")
            , ("PGOPTIONS"        , "options")
            , ("PGAPPNAME"        , "da app")
            , ("PGKEEPALIVES"     , "1")
            , ("PGKEEPALIVESIDLE" , "2")
            , ("PGKEEPALIVESCOUNT", "3")
            , ("PGSSLMODE"        , "on")
            , ("PGREQUIRESSL"     , "1")
            , ("PGSSLCOMPRESSION" , "0")
            , ("PGSSLCERT"        , "ca.cert")
            , ("PGSSLKEY"         , "fda.key")
            , ("PGSSLROOTCERT"    , "root.cert")
            , ("PGREQUIREPEER"    , "yes")
            , ("PGKRBSRVNAME"     , "blah")
            , ("PGGSSLIB"         , "foo")
            , ("PGSERVICE"        , "bar")
            ]

      fmap Map.fromList getEnvironment `shouldReturn` expected



  describe "Options Parser" $ do
    it "parses all options" $ do
      let testArgs = [ "--host=example.com"
                     , "--port=1234"
                     , "--user=nobody"
                     , "--password=everytimeiclosemyeyes"
                     , "--dbname=future"
                     ]

          expected = completeOptions $ mempty
                     { host     = return "example.com"
                     , port     = return 1234
                     , user     = return "nobody"
                     , password = return "everytimeiclosemyeyes"
                     , dbname   = return "future"
                     }

      actual <- withArgs testArgs testParser
      Right actual `shouldBe` expected

    it "parses some and uses defaults for others" $ do
      let testArgs = [ "--user=nobody"
                     , "--password=everytimeiclosemyeyes"
                     , "--dbname=future"
                     ]

          expected = completeOptions $ mempty
                     { host     = return "127.0.0.1"
                     , port     = return 5432
                     , user     = return "nobody"
                     , password = return "everytimeiclosemyeyes"
                     , dbname   = return "future"
                     }

      actual <- withArgs testArgs testParser
      Right actual `shouldBe` expected

    it "parses no options and gives defaults" $ do
      let expected = completeOptions $ mempty
                     { host     = return "127.0.0.1"
                     , port     = return 5432
                     , user     = return "postgres"
                     , password = return ""
                     , dbname   = return ""
                     }

      actual <- withArgs [] testParser
      Right actual `shouldBe` expected
    it "parses the connection string double quoted" $ do
      let testArgs = ["--connectString=\"host=yahoo\""]
          expected = completeOptions $ def { host = return "yahoo" }

      actual <- withArgs testArgs testParser
      Right actual `shouldBe` expected
    it "parses the connection string single quoted" $ do
      let testArgs = ["--connectString='host=yahoo'"]
          expected = completeOptions $ def { host = return "yahoo" }

      actual <- withArgs testArgs testParser
      Right actual `shouldBe` expected
    it "parses the connection string no quotes" $ do
      let testArgs = ["--connectString=host=yahoo"]
          expected = completeOptions $ def { host = return "yahoo" }

      actual <- withArgs testArgs testParser
      Right actual `shouldBe` expected
    it "fails if connectString and other args are passed" $ do
      let testArgs = ["--connectString=host=yahoo", "--port=1234"]
          handler :: ExitCode -> Bool
          handler (ExitFailure 1) = True
          handler _ = False

      shouldThrow (withArgs testArgs testParser) handler

  describe "connection string parser" $ do
    it "fails on empty" $ parseConnectionString "" `shouldBe` Left "MalformedScheme NonAlphaLeading"

    it "parses a single keyword" $ parseConnectionString "host=localhost"
      `shouldBe` Right (mempty { host = return "localhost" })

    it "parses all keywords" $ parseConnectionString
      "host=localhost port=1234 user=jonathan password=open dbname=dev" `shouldBe`
        Right (mempty
          { host = return "localhost"
          , port = return 1234
          , user = return "jonathan"
          , password = return "open"
          , dbname = return "dev"
          })

    it "parses host only connection string" $ parseConnectionString "postgresql://localhost"
      `shouldBe` Right (mempty { host = return "localhost" })

    it "parses all params" $ parseConnectionString "postgresql://jonathan:open@localhost:1234/dev"
      `shouldBe` Right (mempty
          { host = return "localhost"
          , port = return 1234
          , user = return "jonathan"
          , password = return "open"
          , dbname = return "dev"
          })

    it "parses all params using query params" $ parseConnectionString "postgresql:///dev?host=localhost&port=1234"
      `shouldBe` Right (mempty
        { host = return "localhost"
        , port = return 1234
        , dbname = return "dev"
        })

    it "decodes a unix port" $ parseConnectionString "postgresql://%2Fvar%2Flib%2Fpostgresql/dbname"
      `shouldBe` Right (mempty
          { host = return "/var/lib/postgresql"
          , dbname = return "dbname"
          })



