
default_func (x,y) = x * 3 + y * 2

invert :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert f z =[(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

invert2 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert2 f z =[(x,y) | x <- [0..z], y <- [0..z-x], f (x,y) == z]

invert3 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert3 f z = find (0,z) f z
  where
    find (u,v) f z
      | u > z || v < 0 = []
      | z' < z         = find (u+1,v) f z
      | z' == z        = (u,v) : find (u+1,v-1) f z
      | z' > z         = find (u, v-1) f z
        where z' = f (u,v)

invert4 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert4 f z = find (0,m) f z
  where
    find (u,v) f z
      | u > n || v < 0 = []
      | z' < z         = find (u+1,v) f z
      | z' == z        = (u,v) : find (u+1,v-1) f z
      | z' > z         = find (u, v-1) f z
        where z' = f (u,v)
    m  = bsearch (\y -> f (0,y)) (-1,z+1) z
    n  = bsearch (\x -> f (x,0)) (-1,z+1) z

bsearch g (a,b) z
  | a + 1 == b = a
  | g m <= z   = bsearch g (m,b) z
  | otherwise  = bsearch g (a,m) z
    where m = (a + b) `div` 2

