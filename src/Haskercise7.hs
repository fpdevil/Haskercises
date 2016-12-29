{-|
   Author     : Sampath Singamsetty
   Maintainer :
   File       : Haskercise7.hs
   Description: General miscellaneous examples and exercises

-}

module Haskercise7 where

-- required imports
import qualified Data.List      as L
import qualified Data.Map       as M
import           Data.Maybe
import qualified Data.Semigroup as S
-- -----------------------------------------------------------------------------------
-- time series data parsing of financial information
-- the below information will usually come from separate files
info1 :: [(Int, Double)]
info1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9),
         (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2), (12, 201.9)]

info2 :: [(Int, Double)]
info2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5),
         (15, 204.9), (16, 207.1), (18, 210.5), (20, 208.8)]

info3 :: [(Int, Double)]
info3 = [(10, 201.2), (11, 201.6), (12, 201.5), (13, 201.5),
         (14, 203.5), (17, 210.5), (24, 215.1), (25, 218.7)]

info4 :: [(Int, Double)]
info4 = [(26, 219.8), (27, 220.5), (28, 223.8), (29, 222.8),
         (30, 223.8), (31, 221.7), (32, 222.3), (33, 220.8),
         (34, 219.4), (35, 220.1), (36, 220.6)]

-- define a new data type for handling TimeSeries
-- missing values in the TimeSeries is normally represented as NA
data TimeSeries a = TimeSeries [Int] [Maybe a]

-- take data and create time series from the same
-- first get the fulltimes range of times from min to max
crtTS :: [Int] -> [a] -> TimeSeries a
crtTS times vals = TimeSeries fullTimes fullVals
                   where
                     fullTimes  = [(L.minimum times) .. (L.maximum times)]
                     timeValMap = M.fromList (zip times vals)
                     fullVals   = map (`M.lookup` timeValMap) fullTimes

-- auxilliary helper function for timeseries data conversion
convertData :: [(Int, a)] -> TimeSeries a
convertData tv = crtTS times vals
                 where
                   pairs = unzip tv
                   times = fst pairs
                   vals  = snd pairs

-- a function for rendering the time/value pairs
showTV :: (Show a) => Int -> Maybe a -> String
showTV time (Just val) = S.mconcat [show time, " | ", show val, "\n"]
showTV time Nothing    = S.mconcat [show time, " | NA\n"]

-- make TimeSeries data an instance of Show
instance (Show a) => Show (TimeSeries a) where
  show (TimeSeries t v) = S.mconcat rows
                          where
                            rows = zipWith showTV t v

-- convert all the data samples to the format compatible
-- with the data type for TimeSeries
ts1 :: TimeSeries Double
ts1 = convertData info1

ts2 :: TimeSeries Double
ts2 = convertData info2

ts3 :: TimeSeries Double
ts3 = convertData info3

ts4 :: TimeSeries Double
ts4 = convertData info4

-- combine or stitch together the data using Semigroup and Monoid
-- first define helper function for inserting a value of the type
-- (k, Maybe v) pair into a map of the type Map k v.
insertMaybePair :: (Ord k) => M.Map k v -> (k, Maybe v) -> M.Map k v
insertMaybePair tsMap (_, Nothing)    = tsMap
insertMaybePair tsMap (key, Just val) = M.insert key val tsMap

-- stitching together the TimeSeries data
stitchTS :: TimeSeries a -> TimeSeries a -> TimeSeries a
stitchTS (TimeSeries [] []) tseries2 = tseries2
stitchTS tseries1 (TimeSeries [] []) = tseries1
stitchTS (TimeSeries t1 v1) (TimeSeries t2 v2) = TimeSeries allTimes allVals
                                                 where
                                                   combinedTimes = mconcat [t1, t2]
                                                   allTimes      = [(L.minimum combinedTimes) .. (L.maximum combinedTimes)]
                                                   tvMap         = foldl insertMaybePair M.empty (zip t1 v1)
                                                   updMap        = foldl insertMaybePair tvMap (zip t2 v2)
                                                   allVals       = map (`M.lookup` updMap) allTimes


-- make TimeSeries data an instance of Semigroup
instance S.Semigroup (TimeSeries a) where
  (<>) = stitchTS

-- make TimeSeries data an instance of Monoid
instance Monoid (TimeSeries a) where
  mempty = TimeSeries [] []
  mappend = (S.<>)

-- with Monoid instance of TimeSeries data, any number of TimeSeries
-- data can be combined or stitched together
tsAll :: TimeSeries Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- summarizing the data sets; calculate the summary statistics for the
-- data sets interms of finding mean / average value of the data, which
-- would help init finding the highest and lowest values init dataset.
mean :: (Real a) => [a] -> Double
mean xs = total / count
          where
            total = (realToFrac . sum) xs
            count = (realToFrac . length) xs

-- find the mean of TimeSeries data as a whole
meanTS :: (Real a) => TimeSeries a -> Maybe Double
meanTS (TimeSeries _ [])   = Nothing
meanTS (TimeSeries _ vals) = if all (== Nothing) vals
                                then Nothing
                                else Just avg
                                     where
                                       justVals = filter isJust vals
                                       allVals  = map fromJust justVals
                                       avg      = mean allVals
