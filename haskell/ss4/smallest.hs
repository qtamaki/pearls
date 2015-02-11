import Data.Array

{-
smallest :: Ord a => Int -> ([a],[a]) -> a
smallest k (xs,ys) = union (xs,ys) !! k

union (xs,[]) = xs
union ([],ys) = ys
union (x:xs,y:ys) | x < y = x : union (xs, y:ys)
                  | x > y = y : union (x:xs, ys)
-}

-- |[a]| = length [a]
-- (xs ++ ys) !! k = if k < |xs| then xs !! k else ys !! (k - |xs|)

-- union (xs ++ ys, us ++ vs) = union (xs,us) ++ union (ys,vs)

smallest :: Ord a => Int -> ([a],[a]) -> a
smallest k ([],ws) = ws !! k
smallest k (zs,[]) = zs !! k
smallest k (zs,ws) =
  case (a < b,k <= p + q) of
    (True,True)   -> smallest k (zs,us)
    (True,False)  -> smallest (k-p-1) (ys,ws)
    (False,True)  -> smallest k (xs,ws)
    (False,False) -> smallest (k-q-1) (zs,vs)
  where p = (length zs) `div` 2
        q = (length ws) `div` 2
        (xs,a:ys) = splitAt p zs
        (us,b:vs) = splitAt q ws

smallest' :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest' k (xa,ya) = search k (0, m + 1) (0, n + 1)
  where
    (0,m) = bounds xa
    (0,n) = bounds ya
    search k (lx, rx) (ly, ry)
      | lx == rx = ya ! (k + ly)
      | ly == ry = xa ! (k + lx)
      | otherwise =
          case (xa ! mx < ya ! my, k <= mx - lx + my - ly) of
            (True,True)   -> search k (lx,rx) (ly,my)
            (True,False)  -> search (k - (mx - lx) - 1) (mx + 1, rx) (ly, ry)
            (False,True)  -> search k (lx, mx) (ly, ry)
            (False,False) -> search (k - (my - ly) - 1) (lx,rx) (my + 1, ry)
          where mx = (lx + rx) `div` 2
                my = (ly + ry) `div` 2



