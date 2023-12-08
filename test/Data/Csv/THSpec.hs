{-# LANGUAGE TemplateHaskell #-}

module Data.Csv.THSpec where

import Data.Csv hiding (record)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Csv.TH
import GHC.Generics
import Test.Hspec

data Foo = Foo
  { fooName :: Text
  , fooAge :: Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveNamedRecord csvDefaultOptions ''Foo)
$(deriveToAndFromRecord ''Foo)

newtype GenericCsv a = GenericCsv a
  deriving stock (Show, Eq)

instance (Generic a, GToRecord (Rep a) (ByteString, ByteString)) => ToNamedRecord (GenericCsv a) where
  toNamedRecord (GenericCsv a) = genericToNamedRecord defaultOptions a

instance (Generic a, GToRecord (Rep a) ByteString) => ToRecord (GenericCsv a) where
  toRecord (GenericCsv a) = genericToRecord defaultOptions a

instance (Generic a, GFromNamedRecord (Rep a)) => FromNamedRecord (GenericCsv a) where
  parseNamedRecord rec = GenericCsv <$> genericParseNamedRecord defaultOptions rec

instance (Generic a, GFromRecord (Rep a)) => FromRecord (GenericCsv a) where
  parseRecord rec = GenericCsv <$> genericParseRecord defaultOptions rec

spec :: Spec
spec = do
  let foo = Foo "hello" 50
  describe "deriveToNamedRecord" do
    it "is equivalent to the generic" do
      toNamedRecord foo
        `shouldBe` toNamedRecord (GenericCsv foo)
  describe "deriveToRecord" do
    it "is equivalent to the generic" do
      toRecord foo
        `shouldBe` toRecord (GenericCsv foo)
  describe "deriveFromNamedRecord" do
    it "is equivalent to the generic" do
      let record = toNamedRecord foo
          recordG = toNamedRecord (GenericCsv foo)
      runParser (parseNamedRecord record)
        `shouldBe` pure foo
      runParser (parseNamedRecord recordG)
        `shouldBe` pure foo
      runParser (parseNamedRecord record)
        `shouldBe` pure (GenericCsv foo)
      runParser (parseNamedRecord recordG)
        `shouldBe` pure (GenericCsv foo)
  describe "deriveFromRecord" do
    it "is equivalent to the Generic" do
      let record = toRecord foo
          recordG = toRecord (GenericCsv foo)
      runParser (parseRecord record)
        `shouldBe` Right foo
      runParser (parseRecord record)
        `shouldBe` Right (GenericCsv foo)
      runParser (parseRecord recordG)
        `shouldBe` Right foo
      runParser (parseRecord recordG)
        `shouldBe` pure (GenericCsv foo)
