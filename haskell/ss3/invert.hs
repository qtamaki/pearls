
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

invert5 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert5 f z = find (0,m) (n,0) f z
  where
    m  = bsearch (\y -> f (0,y)) (-1,z+1) z
    n  = bsearch (\x -> f (x,0)) (-1,z+1) z
    find (u,v) (r,s) f z
      | u > r || v < s = []
      | v - s <= r - u = rfind (bsearch (\x -> f (x,q)) (u - 1, r + 1) z)
      | otherwise      = cfind (bsearch (\y -> f (p,y)) (s - 1, v + 1) z)
      where
        p = (u + r) `div` 2
        q = (v + s) `div` 2
        rfind p = (if f (p,q) == z then (p,q) : find (u,v) (p - 1, q + 1) f z
                   else find (u,v) (p, q + 1) f z) ++ find (p + 1, q - 1) (r,s) f z
        cfind q = find (u,v) (p - 1, q + 1) f z ++
                  (if f (p,q) == z then (p,q) : find (p + 1, q - 1) (r,s) f z
                   else find (p + 1, q) (r,s) f z)


