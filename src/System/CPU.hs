{-|
Module      : System.CPU
Description : 
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Linux >=2.6

-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , OverloadedStrings
           #-}

module System.CPU where

import Control.Applicative

import Control.DeepSeq

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString as B

import Data.Data

import Data.Word

import GHC.Generics

data CPU = CPU {
    processorID     :: !Word32
  , vendor          :: !B.ByteString
  , model           :: !Word32
  , modelName       :: !B.ByteString
  , revision        :: !Word32
  , microcode       :: !Word32
  , freq            :: !Double
  , cache           :: !Word32
  , physicalID      :: !Word32
  , siblings        :: !Word32
  , coreID          :: !Word32
  , apicID          :: !Word32
  , fpu             :: !Bool
  , fpuExcept       :: !Bool
  , flags           :: [B.ByteString]
  , bogoMIPS        :: !Double
  , cacheAlignment  :: !Word32
  , physicalAddress :: !Word32
  , virtualAddress  :: !Word32
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             )

parsePair :: B.ByteString -> A.Parser a -> A.Parser a
parsePair k vp = A.string k
              *> A.skipSpace
              *> A.char ':'
              *> A.skipSpace
              *> vp

parseProcessor :: A.Parser Word32
parseProcessor = parsePair "processor" A.decimal

parseVendor :: A.Parser B.ByteString
parseVendor = parsePair "vendor" A.takeByteString

parseModel :: A.Parser Word32
parseModel = parsePair "model" A.decimal

parseModelName :: A.Parser B.ByteString
parseModelName = parsePair "model name" A.takeByteString

parseRevision :: A.Parser Word32
parseRevision = parsePair "stepping" A.decimal

parseMicrocode :: A.Parser Word32
parseMicrocode = parsePair "microcode" (A.string "0x" *> A.hexadecimal)

parseFreq :: A.Parser Double
parseFreq = parsePair "cpu MHz" A.double

parseCache :: A.Parser Word32
parseCache = parsePair "cache size"
                       (A.decimal <* (A.skipSpace *> A.string "KB"))

parsePhysicalID :: A.Parser Word32
parsePhysicalID = parsePair "physical id" A.decimal

parseSiblings :: A.Parser Word32
parseSiblings = parsePair "siblings" A.decimal

parseCoreID :: A.Parser Word32
parseCoreID = parsePair "core id" A.decimal

parseApicID :: A.Parser Word32
parseApicID = parsePair "apicid" A.decimal

parseFpu :: A.Parser Bool
parseFpu = parsePair "fpu" parseBool

parseFpuExcept :: A.Parser Bool
parseFpuExcept = parsePair "fpu_exception" parseBool

parseFlags :: A.Parser [B.ByteString]
parseFlags = parsePair "flags" parseWords

parseBogoMIPS :: A.Parser Double
parseBogoMIPS = parsePair "bogomips" A.double

parseCacheAlignment :: A.Parser Word32
parseCacheAlignment = parsePair "cache_alignment" A.decimal

parseAddresses :: A.Parser (Word32, Word32)
parseAddresses = parsePair "address sizes"
                           ((,) <$> parsePhysicalAddress
                                <*> parseVirtualAddress)

parsePhysicalAddress :: A.Parser Word32
parsePhysicalAddress = A.decimal <* A.string " bits physical, "

parseVirtualAddress :: A.Parser Word32
parseVirtualAddress = A.decimal <* A.string " bits virtual"

parseBool :: A.Parser Bool
parseBool = (A.string "yes" *> pure True)
        <|> (A.string "no" *> pure False)

parseWords :: A.Parser [B.ByteString]
parseWords = A.sepBy (A.takeWhile1 (/= ' ')) (A.char ' ')

type TryLines = MaybeT (State [B.ByteString])

runTryLines :: TryLines a -> [B.ByteString] -> Maybe a
runTryLines = evalState . runMaybeT

keepTrying :: A.Parser a -> TryLines a
keepTrying p = do
    lss <- lift get
    case lss of
        []     -> fail ""
        (l:ls) -> case A.parseOnly p l of
            Right r -> lift (put ls) *> pure r
            Left _  -> lift (put ls) *> keepTrying p
