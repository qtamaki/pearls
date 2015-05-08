type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

valExpr :: Expression -> Int
valExpr = sum . map valTerm
valTerm :: Term -> Int
valTerm = product . map valFact
valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

modify x (k,f,t,e) = [(10*k,k*x+f,t,e), (10,x,f*t,e),(10,x,1,f*t+e)]

good c (k,f,t,e) = (f*t+e == c)
ok c (k,f,t,e) = (f*t+e <= c)

solutions c = map fst . filter (good c . snd) . foldr (expand c) []
-- expand x c = filter (ok c . snd) . zip . cross (extend x, modify x) . unzip

expand c x [] = [([[[x]]], (10,x,1,0))]
expand c x evs = concatMap (filter (ok c . snd) . glue x) evs
glue x ((xs:xss):xsss,(k,f,t,e)) =
  [(((x:xs):xss):xsss,(10*k,k*x+f,t,e)),
   (([x]:xs:xss):xsss,(10,x,f*t,e)),
   ([[x]]:(xs:xss):xsss,(10,x,1,f*t+e))]

expressions :: [Digit] -> [Expression]
expressions = concatMap partitions . partitions

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x] : p | p <- ps] ++ [(x:ys):yss | ys:yss <- ps]
                    where ps = partitions xs

mkstr s = foldl1 (\x y -> x ++ s ++ y)
showFact xs = mkstr "" $ map show xs

showTerm xs = mkstr "x" $ map showFact xs

showExpr xs = mkstr "+" $ map showTerm xs

showAll = mapM_ print $ map (\x -> "100=" ++ showExpr x) . solutions 100 $ [1..9]


