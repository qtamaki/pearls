data Datum = Datum --
type Data = [Datum]
data Candidate = Candidate
data Value = Value

-- candidates :: [Datum] -> [Candidate]
candidates :: Data -> [Candidate]
value :: Candidate -> Value
good :: Value -> Bool

solutions :: Data -> [Candidate]
solutions = filter (good . value) . candidates

candidates = foldr extend []

extend :: Datum -> [Candidate] -> [Candidate]

filter (good . value) = filter (good . value) . filter (ok . value) -- 6.2

filter (ok . value) . extend x = filter (ok . value) . extend x . filter (ok . value) -- 6.3

--
= solutions
= filter (good . value) . candidates
= filter (good . value) . foldr extend []
= filter (good . value) . filter (ok . value) . foldr expand []
= filter (good . value) . foldr extend' []

-- f :: [Candidate] -> [Candidate]
-- f = filter (ok . value)
-- g :: datum -> [Candidate] -> [Candidate]
-- g = extend
-- h :: datum -> [Candidate] -> [Candidate]
-- h = extend'
-- f (g datum [candidate]) = h datum (f [candidate])

extend' :: Datum -> [Candidate] -> [Candidate]

map value . expand x = modify x . map value -- 6.4

candidates = map (fork (id, value)) . foldr extend' []

fork (f,g) x = (f x, g x)

map (fork (id, value)) . extend' x = expand x . map (fork (id, value))

candidates = foldr expand []

fst . fork (f,g) = f かつ snd . fork (f,g) = g -- 6.5

fork (f,g) h = fork (f.h, g.h) -- 6.6

cross (f.g) (x.y) = (f x, g y)

fork (f.h, g.k) = cross (f,g) . fork (h,k) -- 6.7

unzip :: [(a,b)] -> ([a],[b])
unzip = fork (map fst, map snd)

-- zip::([a],[b]) -> [(a,b)]
-- zip.unzip = id

= unzip . map (fork (f,g))
= fork (map fst, map snd) . map (fork (f,g))
-- fork (map fst . map (fork (f,g)), map snd . map (fork (f,g)))
= fork (map (fst . fork (f,g)), map (snd . fork (f,g))) -- 6.6, map (f.g) = map f . map g
= fork (map f, map g) -- 6.5

fork (map f, map g) = unzip . map (fork (f,g)) -- 6.8

-- zip . unzip . map (fork (f,g)) = zip . fork (map f, map g)
map (fork (f,g)) = zip . fork (map f, map g) -- 6.9

map (fork (f,g)) . filter (p.g) = filter (p.snd) . map (fork (f,g)) -- 6.10

= map (fork (id, value)) . extend' x
= map (fork (id, value)) . filter (ok . value) . extend x -- extend'の定義
= filter (ok.snd) . map (fork (id, value)) . extend x -- 6.10

= map (fork (id, value)) . extend x
= zip . fork (id, map value) . extend x -- 6.9, map id = id
= zip . fork (extend x, map value . expand x) -- 6.6
= zip . fork (extend x, modify x . map value) -- 6.4
-- = zip . fork (extend x . id , modify x . map value)
= zip . cross (extend x, modify x) . fork (id, map value) -- 6.7
-- = zip . cross (extend x, modify x) . fork (map id, map value)
= zip . cross (extend x, modify x) . unzip . map (fork (id, value)) -- 6.8

solutions = map fst . filter (good.snd) . foldr expand []
expand = filter (ok.snd) . zip . cross (extend x, modify x) . unzip


