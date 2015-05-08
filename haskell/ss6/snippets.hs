data Datum = Datum
type Data = [Datum]
data Candidate = Candidate
data Value = Value

candidates :: Data -> [Candidate]
candidates = foldr extend []

-- candidates = map (fork (id, value)) . foldr extend' []
-- map (fork (id, value)) . extend' x = expand x . map (fork (id, value))

value :: Candidate -> Value
value = undefined

good :: Value -> Bool
good = undefined

extend :: Datum -> [Candidate] -> [Candidate]
extend = undefined

ok :: Value -> Bool
ok = undefined

extend' :: Datum -> [Candidate] -> [Candidate]
extend' x = filter (ok . value) . foldr extend x

solutions = filter (good . value) . foldr extend' []

fork = undefined

-- map value . extend x = modify x . map value


