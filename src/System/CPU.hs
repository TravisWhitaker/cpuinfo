{-|
Module      : System.CPU
Description : Haskell Library for Checking CPU Information
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

module System.CPU (
    CPU(..)
    -- * Retrieving CPU Information
  , getCPUs
  , tryGetCPUs
    -- * Physical Features
  , physicalProcessors
  , physicalCores
  , logicalCores
  , hyperthreadingFactor
  , hyperthreadingInUse
  ) where

import Control.Applicative

import Control.Arrow

import Control.DeepSeq

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

import Data.Data

import qualified Data.Set as S

import Data.Maybe

import Data.Word

import GHC.Generics

-- | Representation of a logical processor and its features.
data CPU = CPU {
    -- | Logical Processor Index
    processorID     :: !Word32
    -- | CPU Vendor
  , vendor          :: !B.ByteString
    -- | CPU Model Number
  , model           :: !Word32
    -- | CPU Model Name
  , modelName       :: !B.ByteString
    -- | CPU Model Revision
  , revision        :: !Word32
    -- | CPU Microcode Revision
  , microcode       :: !Word32
    -- | Processor Frequency
  , freq            :: !Double
    -- | CPU Cache Size. (TODO figure out how to get the cache topology)
  , cache           :: !Word32
    -- | Physical Processor Index
  , physicalID      :: !Word32
    -- | Number of Physical Cores on this Physical Processor.
  , siblings        :: !Word32
    -- | Physical Core Index
  , coreID          :: !Word32
    -- | CPU APIC Index
  , apicID          :: !Word32
    -- | Whether or not the Physical Core provides a floating point unit.
  , fpu             :: !Bool
    -- | Whether or not the Physical Core provides a floating point exception
    --   unit.
  , fpuExcept       :: !Bool
    -- | Vendor-specific CPU flags.
  , flags           :: [B.ByteString]
    -- | MIPS approximation computed by the Linux kernel on boot.
  , bogoMIPS        :: !Double
    -- | Cache line size in bytes.
  , cacheAlignment  :: !Word32
    -- | Physical address width.
  , physicalAddress :: !Word32
    -- | Virtual address width.
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
parseVendor = parsePair "vendor_id" A.takeByteString

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

splitCPULines :: B.ByteString -> [[B.ByteString]]
splitCPULines = splitCPUs . BC.lines
    where splitCPUs [] = []
          splitCPUs ls = let (cs, lss) = break (== "") ls
                         in cs : case lss of []      -> []
                                             (_:ls') -> splitCPUs ls'

tryCPU :: TryLines CPU
tryCPU = do
    proc     <- keepTrying parseProcessor
    vend     <- keepTrying parseVendor
    modl     <- keepTrying parseModel
    modn     <- keepTrying parseModelName
    rev      <- keepTrying parseRevision
    mcode    <- keepTrying parseMicrocode
    frq      <- keepTrying parseFreq
    cch      <- keepTrying parseCache
    pid      <- keepTrying parsePhysicalID
    sib      <- keepTrying parseSiblings
    cid      <- keepTrying parseCoreID
    aid      <- keepTrying parseApicID
    flpu     <- keepTrying parseFpu
    flpex    <- keepTrying parseFpuExcept
    flg      <- keepTrying parseFlags
    bgm      <- keepTrying parseBogoMIPS
    ca       <- keepTrying parseCacheAlignment
    (pa, va) <- keepTrying parseAddresses
    pure $ CPU proc
               vend
               modl
               modn
               rev
               mcode
               frq
               cch
               pid
               sib
               cid
               aid
               flpu
               flpex
               flg
               bgm
               ca
               pa
               va

-- | Read @/proc/cpuinfo@ and try to parse the output. If this function returns
--   'Nothing' on your system, please file a bug report with your
--   @/proc/cpuinfo@ contents and CPU specifications.
tryGetCPUs :: IO (Maybe [CPU])
tryGetCPUs = (mapM (runTryLines tryCPU) . splitCPULines)
          <$> B.readFile "/proc/cpuinfo"

-- | Read @/proc/cpuinfo@ and try to parse the output. If this function throws
--   an error on your system, please file a bug report with your @/proc/cpuinfo@
--   contents and CPU specifications.
getCPUs :: IO [CPU]
getCPUs = fromMaybe (error e) <$> tryGetCPUs
    where e = unlines [ "Couldn't parse your /proc/cpuinfo contents."
                      , "Please file a bug including your /proc/cpuinfo here:"
                      , "https://github.com/traviswhitaker/cpuinfo/issues"
                      ]

unique :: Ord a => [a] -> Int
unique = S.size . S.fromList

-- | Counts the number of physical processors in the system. A physical
--   processor corresponds to a single CPU unit in a single socket, i.e. unless
--   you have a multi-socket motherboard, this number will be one.
physicalProcessors :: [CPU] -> Int
physicalProcessors = unique . map physicalID

-- | Counts the number of physical cores in the system. A physical core is an
--   independent processing unit that reads and executes instructions on its
--   own, but potentially shares its die (and other resources) with other cores.
physicalCores :: [CPU] -> Int
physicalCores = unique . map (physicalID &&& coreID)

-- | Counts the number of logical cores in the system. A logical core is a
--   virtual processing unit exposed to the operating system, that may or may
--   not directly correspond with an independent physical processing unit, e.g.
--   a hyperthread appears as an independent processing unit to the operating
--   system, but has no physically dedicated execution resources.
logicalCores :: [CPU] -> Int
logicalCores = length

-- | The hyperthreading factor is the number of logical cores divided by the
--   number of physical cores. This quantity indicates the degree to which
--   physical execution resources are shared among logical processors, and may
--   be used to tune parallel applications.
hyperthreadingFactor :: [CPU] -> Rational
hyperthreadingFactor cpus = (fromIntegral (logicalCores cpus))
                          / (fromIntegral (physicalCores cpus))

-- | If hyperthreading is in use, the 'hyperthreadingFactor' will be greater
--   than 1.
hyperthreadingInUse :: [CPU] -> Bool
hyperthreadingInUse = (/= 1) . hyperthreadingFactor
