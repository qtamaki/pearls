
default_func (x,y) = x * x `div` 3 + y - 3

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

invert_trace invs = rev $ foldl fun ([],[]) invs
                 where fun (xs, ys) res = case res of
                                            Right(x) -> (x:xs, ys)
                                            Left(y) -> (xs, y:ys)
                       rev (x,y) = (reverse x, reverse y)

pp_invert (xs,ys) = do
    mapM_ putStrLn $ fmap (pp "\t") xs
    mapM_ putStrLn $ fmap (pp "\t\t") ys
    where pp d (zx,zy) = show zx ++ d ++ show zy

main =  pp_invert $ invert_trace $ invert4 default_func 20

