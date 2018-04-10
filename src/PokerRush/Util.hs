module PokerRush.Util where

import Data.List

class Default a where
    def :: a

choose :: Integral t => t -> t -> t
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

replace pos new list = take pos list ++ new : drop (pos + 1) list
replaceApp pos f list = take pos list ++ f (list !! pos) : drop (pos + 1) list

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

combinations' :: Ord a => Int -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' _ [] = []
combinations' n l =
    -- filter (\l -> (length . nub) l == length l) $
    -- nub $ sort $ map sort $ sequence (replicate k l)

    if n > length l then []
    else do
        e <- l
        let rst = delete e l
        map (e:) (combinations (n - 1) rst) ++ combinations n rst

combinations n l =
    map sort $ map (map (l !!)) idxs
    where idxs = nub $ sort $ map sort $ combinations' n [ 0 .. length l - 1 ]

-- remove the first occurence of all elements in l2 from l1
deductList :: Eq a => [a] -> [a] -> [a]
deductList l1 l2 = foldr delete l1 l2

contains :: Eq a => [a] -> [a] -> Bool
contains l1 l2 =
    length l3 == length l1 - length l2
    where l3 = deductList l1 l2

groupList' :: Eq a => [a] -> [(a, Int)]
groupList' [] = []
groupList' (c:cs) =
    (c, length cs - length rst + 1) : groupList' rst
    where rst = dropWhile (== c) cs

-- sort a list to (card, number) pairs
groupList :: Ord a => [a] -> [(a, Int)]
groupList list = groupList' (sort list)

combinePossOr :: [Double] -> Double
combinePossOr ps =
    foldl (\p0 p1 -> p0 + p1 - p0 * p1) 0 ps

u = undefined
inf = read "Infinity" :: Double
