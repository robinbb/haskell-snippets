-- Copyright 2015 Robin Bate Boerop <me@robinbb.com>

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main where

import Data.List (sortBy, length, sort)
import Test.QuickCheck

orderCounted :: Bool -> (Int, a) -> (Int, a) -> Ordering
orderCounted ascending (a, _) (b, _) =
   case (compare a b) of
     LT -> if ascending then LT else GT
     GT -> if ascending then GT else LT
     EQ -> EQ
     
prop_orderCountedAscending asc (a :: (Int, ())) (b :: (Int, ())) =
   asc ==>
      if fst a == fst b
         then orderCounted asc a b == EQ
         else orderCounted asc a b == if fst a < fst b then LT else GT

prop_orderCountedDescending asc (a :: (Int, ())) (b :: (Int, ())) =
   not asc ==>
      if fst a == fst b
         then orderCounted asc a b == EQ
         else orderCounted asc a b == if fst a < fst b then GT else LT

sortCounted :: Bool -> [(Int, a)] -> [(Int, a)]
sortCounted ascending =
   sortBy (orderCounted ascending)

prop_sortCountedLengthsMatch asc (xs :: [(Int, ())]) =
   l == length xs
   where
      l = length $ sortCounted asc xs

prop_sortCountedAscending asc (xs :: [(Int,())]) =
   asc ==> l == r
   where
      l = map fst $ sortCounted asc xs
      r = sort $ map fst xs

prop_sortCountedDescending asc (xs :: [(Int, ())]) =
   not asc ==> l == r
   where
      l = map fst $ sortCounted asc xs
      r = reverse $ sort $ map fst xs

prop_sortCountedPreservesPairing asc (li :: [Int]) =
   pairsHaveSameNum new && pairsHaveSameNum (sortCounted asc new)
   where new = map (\count -> (count, replicate count count)) li
         pairsHaveSameNum []         = True
         pairsHaveSameNum ((c, l):r) = all (c ==) l && pairsHaveSameNum r

listLengths :: [[a]] -> [(Int, [a])]
listLengths =
   map (\l -> (length l, l))

prop_listLengthsEqualContents (lists :: [[()]]) =
   and $ map (\(c, l) -> c == length l) $ listLengths lists

prop_listLengthsIntContents (lists :: [[Int]]) =
   and $ map (\(c, l) -> c == length l) $ listLengths lists

sortByListSize :: Bool -> [[a]] -> [[a]]
sortByListSize ascending =
   map snd . sortCounted ascending . listLengths

isSortedByListSize :: Bool -> [[a]] -> Bool
isSortedByListSize asc l =
   and $ zipWith
            (\a b -> if asc then length a <= length b
                            else length a >= length b)
            l (tail l)
          
prop_sortByListSizeToughList asc =
   let toughList =
          [ [], [], []
          , [1], [2], [3]
          , [4, 5], [6, 7]
          , [8, 9, 10], [11, 12, 13]
          , [14, 15, 16, 17, 18]
          ]
       permutes = shuffle toughList
    in forAll permutes $ \l ->
          isSortedByListSize asc (sortByListSize asc l)

prop_sortByListLizeUnit asc (l :: [[()]]) =
   isSortedByListSize asc $ sortByListSize asc l

prop_sortByListLizeInt asc (l :: [[Int]]) =
   isSortedByListSize asc $ sortByListSize asc l

prop_ParticularResult =
   sortByListSize True
      [ [1, 2]
      , [3, 4, 5, 6]
      , []
      , [7, 8, 9]
      ]
   == [ []
      , [1, 2]
      , [7, 8, 9]
      , [3, 4, 5, 6]
      ]

return []  -- Ridiculous TH hack. Necessary to make quickCheckAll work.

main = $quickCheckAll
