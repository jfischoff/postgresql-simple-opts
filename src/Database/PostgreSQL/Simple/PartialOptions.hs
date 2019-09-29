{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}
{-# LANGUAGE RecordWildCards, LambdaCase, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE CPP, GADTs, OverloadedStrings #-}
module Database.PostgreSQL.Simple.PartialOptions
  ( PartialOptions(..)
  , parseConnectionString
  , completeParser
  , completeOptions
  ) where
import Database.PostgreSQL.Simple.PartialOptions.Internal
