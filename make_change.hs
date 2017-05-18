import Control.Monad.State
import Data.List

type Coins = [Int]

type StMach a = StateT Coins IO a

make_change :: Coins -> Int -> IO () -- IO (Int, Coins) 
make_change coins amt = runStateT (recurs amt) coins *> return ()

-- author: Monad Book
recurs :: Int -> StMach Int
recurs amt = 
	if amt == 0
		then return amt
		else (next_coin amt >>= recurs)	

next_coin :: Int -> StMach Int
next_coin amt = do 
	innerS <- get
	lift $ putStrLn $ "(" ++ (show amt) ++ ", " ++ (show innerS) ++ ")"	
	s      <- state $ \coins -> 
		 	case maxChangeCoin coins amt of
				Just m  -> (amt - m, delete m coins)
				Nothing -> (amt, removeMaxCoin coins)
	return s

maxChangeCoin :: Ord a => [a] -> a -> Maybe a
maxChangeCoin xs a =  
 let 
   filtered = filter (<= a) xs
 in 
   case filtered of 
   	[] -> Nothing
   	as -> Just (maximum as)

removeMaxCoin :: Ord a => [a] -> [a]
removeMaxCoin [] = []
removeMaxCoin xs = delete (maximum xs) xs

-- author: Monad Book
dispense :: Int -> StMach ()
dispense i = 
   ( lift $ 
      putStrLn ( "dispense "
                  ++(show i)
                  ++" cents" )
   )
   where 
     cents = if i==1 
                then " cent" 
                else " cents"

-- credit for help: http://stackoverflow.com/a/44029311/409976
f :: Int -> StateT [Int] IO Int
f i = state $ \xs -> case xs of
	[]     -> (i, [])
	(a:as) -> (i+a, as)

update :: Int -> [Int] -> (Int, [Int])      
update x []     = (x, [])
update x (y:ys) = (x+y, ys)        