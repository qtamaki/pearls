import Data.List

sortsum :: (Ord a, Num a) => [a] -> [a] -> [a]
sortsum xs ys = sort [x + y | x <- xs, y <- ys]

-- x - y = x + negate y

type Label a = (a,(Int,Int))

subs :: (Num a) => [a] -> [a] -> [Label a]
subs xs ys = [(x - y, (i,j)) | (x,i) <- zip xs [1..],(y,j) <- zip ys [1..]]

sortsums xs ys = map fst (sortsubs xs (map negate ys))
sortsubs xs ys = sort (subs xs ys)

table :: (Ord a,Num a) => [a] -> [a] -> [(Int,Int,Int)]
table xs ys = map snd (map (tag 1) xxs `merge` map (tag 2) yys)
              where xxs = sortsubs xs xs
                    yys = sortsubs ys ys
tag i (x,(j,k)) = (x,(i,j,k))

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys


