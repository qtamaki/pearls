
default_func (x,y) = x + y

invert :: ((Int,Int) -> Int) -> Int -> [Either (Int,Int) (Int,Int)]
invert f z = do
                x <- [0..z]
                y <- [0..z]
                return $ case f (x,y) of
                  n | n == z     -> Right((x,y))
                    | otherwise  -> Left((x,y))

invert_trace invs = rev $ foldl fun ([],[]) invs
                 where fun (xs, ys) res = case res of
                                            Right(x) -> (x:xs, ys)
                                            Left(y) -> (xs, y:ys)
                       rev (x,y) = (reverse x, reverse y)

pp_invert (xs,ys) = do
    mapM_ putStrLn $ fmap pp xs
    mapM_ putStrLn $ fmap pp ys
    where pp (zx,zy) = show zx ++ "\t" ++ show zy

main =  pp_invert $ invert_trace $ invert default_func 10

