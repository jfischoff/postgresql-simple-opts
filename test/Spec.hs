{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Database.PostgreSQL.Simple.Options
import Database.PostgreSQL.Simple
import System.Environment
import Options.Applicative
import System.Exit

testParser :: IO Options
testParser = execParser $ info parser mempty

main :: IO ()
main = hspec $ describe "Options Parser" $ do 
  it "parses all options" $ do 
    let testArgs = [ "--host=example.com"
                   , "--port=1234"
                   , "--user=nobody"
                   , "--password=everytimeiclosemyeyes"
                   , "--database=future"
                   ]
    
        expected = OConnectInfo $ ConnectInfo 
                   { connectHost     = "example.com"
                   , connectPort     = 1234
                   , connectUser     = "nobody"
                   , connectPassword = "everytimeiclosemyeyes"
                   , connectDatabase = "future"
                   }
                   
    actual <- withArgs testArgs testParser
    actual `shouldBe` expected
  
  it "parses some and uses defaults for others" $ do 
    let testArgs = [ "--user=nobody"
                   , "--password=everytimeiclosemyeyes"
                   , "--database=future"
                   ]
    
        expected = OConnectInfo $ ConnectInfo 
                   { connectHost     = "127.0.0.1"
                   , connectPort     = 5432
                   , connectUser     = "nobody"
                   , connectPassword = "everytimeiclosemyeyes"
                   , connectDatabase = "future"
                   }
                   
    actual <- withArgs testArgs testParser
    actual `shouldBe` expected
  
  it "parses no options and gives defaults" $ do 
    let expected = OConnectInfo $ ConnectInfo 
                   { connectHost     = "127.0.0.1"
                   , connectPort     = 5432
                   , connectUser     = "postgres"
                   , connectPassword = ""
                   , connectDatabase = ""
                   }
                   
    actual <- withArgs [] testParser
    actual `shouldBe` expected
  it "parses the connection string double quoted" $ do
    let testArgs = ["--connectionString=\"a b\""]
        expected = OConnectString "a b"
                   
    actual <- withArgs testArgs testParser
    actual `shouldBe` expected
  it "parses the connection string single quoted" $ do
    let testArgs = ["--connectionString='a b'"]
        expected = OConnectString "a b"

    actual <- withArgs testArgs testParser
    actual `shouldBe` expected
  it "parses the connection string no quotes" $ do
    let testArgs = ["--connectionString=a_b"]
        expected = OConnectString "a_b"

    actual <- withArgs testArgs testParser
    actual `shouldBe` expected
  it "fails if connectionString and other args are passed" $ do
    let testArgs = ["--connectionString=a_b", "--port=1234"]
        handler :: ExitCode -> Bool
        handler (ExitFailure 1) = True
        handler _ = False
      
    shouldThrow (withArgs testArgs testParser) handler