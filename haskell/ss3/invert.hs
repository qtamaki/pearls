import Control.Monad.Writer

default_func (x,y) = x * 3 + y * 2

invert :: ((Int,Int) -> Int) -> Int -> [Either (Int,Int) (Int,Int)]
invert f z = do
                x <- [0..z]
                y <- [0..z]
                return $ case f (x,y) of
                  n | n == z     -> Right((x,y))
                    | otherwise  -> Left((x,y))

invert2 :: ((Int,Int) -> Int) -> Int -> [Either (Int,Int) (Int,Int)]
invert2 f z = do
                x <- [0..z]
                y <- [0..z-x]
                return $ case f (x,y) of
                  n | n == z     -> Right((x,y))
                    | otherwise  -> Left((x,y))

invert3 :: ((Int,Int) -> Int) -> Int -> [Either (Int,Int) (Int,Int)]
invert3 f z = find (0,z) f z
  where
    find (u,v) f z
      | u > z || v < 0 = []
      | z' < z         = Left((u,v)) : find (u+1,v) f z
      | z' == z        = Right((u,v)) : find (u+1,v-1) f z
      | z' > z         = Left((u,v)) : find (u, v-1) f z
        where z' = f (u,v)

invert4 :: ((Int,Int) -> Int) -> Int -> [Either (Int,Int) (Int,Int)]
invert4 f z = find (0,m) f z
  where
    find (u,v) f z
      | u > n || v < 0 = []
      | z' < z         = Left((u,v)) : find (u+1,v) f z
      | z' == z        = Right((u,v)) : find (u+1,v-1) f z
      | z' > z         = Left((u,v)) : find (u, v-1) f z
        where z' = f (u,v)
    m  = bsearch (\y -> f (0,y)) (-1,z+1) z
    n  = bsearch (\x -> f (x,0)) (-1,z+1) z

bsearch g (a,b) z
  | a + 1 == b = a
  | g m <= z   = bsearch g (m,b) z
  | otherwise  = bsearch g (a,m) z
    where m = (a + b) `div` 2

type Wt = Writer [(Int,Int)]
type WriteFunc = (Int,Int) -> Wt Int

write_func :: WriteFunc
write_func (x,y) = writer $ let z = default_func (x,y) in (z, [(x,y)])

invert4' :: WriteFunc -> Int -> Wt [(Int,Int)]
invert4' f z = do
  m <- bsearch' (\y -> f (0,y)) (-1,z+1) z
  find (0,m) f z
    where
    find :: (Int,Int) -> WriteFunc -> Int -> Wt [(Int,Int)]
    find (u,v) f z = do 
      n <- bsearch' (\x -> f (x,0)) (-1,z+1) z
      z' <- f (u,v)
      if u > n || v < 0 then return []
        else if z' < z then find (u+1,v) f z
          else if z' == z then do xs <- find (u+1,v-1) f z
                                  return ((u,v):xs)
            else if z' > z then find (u, v-1) f z
              else return []

bsearch' :: (Int -> Wt Int) -> (Int,Int) -> Int -> Wt Int
bsearch' g (a,b) z
  | a + 1 == b = return a
  | otherwise = do x <- g m
                   if x <= z then bsearch' g (m,b) z
                             else bsearch' g (a,m) z
    where m = (a + b) `div` 2

invert_trace invs = rev $ foldl fun ([],[]) invs
                 where fun (xs, ys) res = case res of
                                            Right(x) -> (x:xs, ys)
                                            Left(y) -> (xs, y:ys)
                       rev (x,y) = (reverse x, reverse y)

pp_invert (xs,ys) = do
    mapM_ putStrLn $ fmap (pp "\t") ys
    mapM_ putStrLn $ fmap (pp "\t\t") xs
    putStrLn ""
    where pp d (zx,zy) = show zx ++ d ++ show zy

main =  mapM_ pp_invert $ fmap (\f -> invert_trace $ f default_func 20) [invert, invert2, invert3, invert4]

-- main = pp_invert $ runWriter $ invert4' write_func 20
