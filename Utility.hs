module Utility
where

import Data.List 
import Control.Monad
import Control.Monad.ST
-- import Control.Monad.Random
import Data.Array.ST
import GHC.Arr
import Debug.Trace
import qualified Data.Set as Set


removeMaybe:: Maybe Int -> Int
removeMaybe mi = case mi of 
   Nothing -> 0
   Just i -> i


implies :: Bool ->Bool ->Bool
implies a b =  not (a &&  not b)

forall, exists :: [a] -> (a -> Bool) -> Bool
forall xs p = all p xs 
exists xs p = any p xs

difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = foldr (\x a -> if elem x ys then a else x:a) [] xs

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

cartesian :: [a]->[a]->[(a,a)]
cartesian a b = [(x,y)| x <- a, y <- b]

subList :: Eq a => [a] -> [a] -> Bool
subList a b = a `elem` (powerList b)

subset :: Eq a =>[a] -> [a] -> Bool
subset xs ys = and [elem x ys | x <- xs]

subsets s n = (subsets_stream s) !! n

subsets_stream []     = [[]] : repeat []
subsets_stream (x:xs) =
    let r = subsets_stream xs
        s = map (map (x:)) r
    in [[]] : zipWith (++) s (tail r)


eachList :: [a] -> [[a]]
eachList x = [[y]|y<-x]


head2 :: Eq a => [[a]] -> [a]
head2 x 
 | x == [] = []
 | otherwise = head x

head3 [] = ""
head3 (x:_) =x

last2 :: Eq a => [[a]] -> [a]
last2 x 
 | x == [] = []
 | otherwise = last x

showSet :: Show a => [a] -> String -> String
showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

rvsort :: Ord a => [a] -> [a]
rvsort x = reverse(sort x )

fst3 ( s, _, _) = s
snd3 ( _ ,s, _) = s
thrd3 ( _ ,_ ,s) = s 

fst4 ( s, _, _, _) = s
snd4 ( _ ,s, _, _) = s
thrd4 ( _ ,_ ,s, _) = s 
frth4 ( _ ,_ , _,s) = s 

deletes s l  = if s `notElem` l
               then l
               else deletes s (delete s l) 

combinations :: [[a]] -> [[a]]
combinations t = case t of []    -> [[]]
                           aa:uu -> [a:u | a <- aa, u <- combinations uu]

words2 :: [String] -> String
words2 ([]) = []
words2 (s:[]) = s
words2 (s:ss) = s++ "," ++ words2 ss

(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) a b = True

rotate :: Eq a => [a] -> [[a]]
rotate [] =  [[]] 
rotate x =  rot x x 
   where rot (n:ns) y =  if   (ns++[n]) /= y
                         then [ns++[n]] ++ (rot (ns++[n]) y)
                         else [ns++[n]]
proj :: Int -> [a] -> a
proj n ls = ls !! n

listing x = [x]

justTrue f x = case f x of Just _     -> True 
                           otherwise  -> False

justList f x = case f x of Just ss    -> ss
                           otherwise  -> []


{- example
main> concat2 ["a", "fa"]
"a,fa"-}

rejectJust x = case x of Just y    -> y
                  --       Nothing -> x

show2 :: [String] -> String
show2 a = case a of [] -> "{}"
                    x -> "{"++show2a x
 where show2a x = case x of  [] -> "}"
                             xs:xss -> case xss of [] -> xs++"}"
                                                   xss -> xs++","++ (show2a xss)
{- example
main> show2 ["a", "fasd"]
"{a,fasd}"  -}

func :: Eq a => [(a, t)] -> a -> t
func setpair fstag = head[rejectJust$fu z fstag | z<-setpair]
 where fu (a,y) x = if x == a then  Just y else Nothing

singleton x = case x of 
                [a] -> True
                otherwise -> False


duplicate :: [a] -> Int -> [a]
duplicate fs n = case n of 
                  0 -> fs
                  1 -> fs
                  n -> duplicate (fs ++ [head fs]) (n-1)

-- shuffle :: (Eq a)=> [a] -> StdGen ->  [a]
-- shuffle [] gen =  []
-- shuffle lst gen = shuffle' lst gen []
--  where 
--  shuffle' lst gen rest = if not (null lst)
--                    then   let (n,gen2) = randomR (0, (length lst)-1) gen
--                           in shuffle' (delete (lst !! n) lst) gen2  ([lst !! n] ++ rest){--}
--                    else rest

dtail x = tail $ tail x
nsort x = nub (sort x)

replace :: Char -> String -> String-> String 
replace x y str = str >>= (\c -> if c == x then y else [c])

rep x = replace ']' ")+" (replace '[' "(" x)


trace2 x= trace ("= " ++ show x)

concat2 :: [String] -> String
concat2 a = case a of 
            [] -> []
            x:xs -> x ++ concat2' xs
             where concat2' [] = ""
                   concat2' xs = ","++ (concat2 xs)--head

snub ::  Ord a => [a] -> [a]
snub  = Set.toAscList. Set.fromList
