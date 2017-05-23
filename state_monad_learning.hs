import Control.Monad.State

-- credit for help: http://stackoverflow.com/a/44029311/409976
f :: Int -> StateT [Int] IO Int
f i = state $ \xs -> case xs of
	[]     -> (i, [])
	(a:as) -> (i+a, as)

update :: Int -> [Int] -> (Int, [Int])      
update x []     = (x, [])
update x (y:ys) = (x+y, ys)        