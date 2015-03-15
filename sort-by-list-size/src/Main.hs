import Data.List (sortBy, length)

orderCounted :: Bool -> (Int, a) -> (Int, a) -> Ordering
orderCounted ascending (a, _) (b, _) =
   case (compare a b) of
     LT -> if ascending then LT else GT
     GT -> if ascending then GT else LT
     EQ -> EQ
     
sortCounted :: Bool -> [(Int, a)] -> [(Int, a)]
sortCounted ascending =
   sortBy (orderCounted ascending)

listLengths :: [[a]] -> [(Int, [a])]
listLengths =
   map (\l -> (length l, l))

sortByListSize :: Bool -> [[a]] -> [[a]]
sortByListSize ascending =
   map snd . sortCounted ascending . listLengths

main =
   print $ sortByListSize True
      [ [1, 2]
      , [3, 4, 5, 6]
      , []
      , [7, 8, 9]
      ]
