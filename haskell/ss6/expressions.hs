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

good :: Int -> Bool
good v = (v == 100)

expressions :: [Digit] -> [Expression]
-- expressions = concatMap partitions . partitions
expressions = foldr extend []

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x] : p | p <- ps] ++ [(x:ys):yss | ys:yss <- ps]
                    where ps = partitions xs

extend :: Digit -> [Expression] -> [Expression]
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es
glue :: Digit -> Expression -> [Expression]
glue x ((xs:xss):xsss) = [((x:xs):xss):xsss,
                           ([x]:xs:xss):xsss,
                           [[x]]:(xs:xss):xsss]

mkstr s = foldl1 (\x y -> x ++ s ++ y)
showFact xs = mkstr "" $ map show xs

showTerm xs = mkstr "x" $ map showFact xs

showExpr xs = mkstr "+" $ map showTerm xs

showAll = mapM_ print $ map (\x -> "100=" ++ showExpr x) $ filter (good.valExpr) . expressions $ [1..9]


