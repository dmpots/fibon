{-# LANGUAGE DeriveDataTypeable #-}
module Fibon.Result (
    FibonResult(..)
  , RunData(..)
  , BuildData(..)
  , RunSummary(..)
  , RunDetail(..)
  , ExtraStats
  , noBuildData
)
where
import Data.ByteString(ByteString)
import Data.Serialize
import Data.Generics

data FibonResult = FibonResult {
      benchName   :: String
    , buildData   :: BuildData
    , runData     :: RunData
  } deriving(Read, Show, Data, Typeable)

data BuildData = BuildData {
      buildTime :: Double  -- ^ Time to build the program
    , buildSize :: String  -- ^ Size of the program
  }
  deriving(Read, Show, Data, Typeable)

noBuildData :: BuildData
noBuildData  = BuildData 0 ""

data RunData = RunData {
    summary :: RunSummary
  , details :: [RunDetail]
  } deriving(Read, Show, Data, Typeable)

data RunSummary = RunSummary {
      meanTime     :: Double
    , stdDevTime   :: Double
    , statsSummary :: ExtraStats
  }
  deriving (Read, Show, Data, Typeable)

data RunDetail = RunDetail {runTime :: Double, runStats :: ExtraStats}
  deriving (Read, Show, Data, Typeable)

--type ExtraStats = [(String, String)]
type ExtraStats = ByteString


--
-- Binary Instances
--
instance Serialize Fibon.Result.FibonResult where
  put (FibonResult a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (FibonResult a b c)

instance Serialize Fibon.Result.BuildData where
  put (BuildData a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (BuildData a b)

instance Serialize Fibon.Result.RunData where
  put (RunData a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (RunData a b)

instance Serialize Fibon.Result.RunDetail where
  put (RunDetail a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (RunDetail a b)

instance Serialize Fibon.Result.RunSummary where
  put (RunSummary a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (RunSummary a b c)

