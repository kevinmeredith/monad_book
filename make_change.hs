import Control.Monad.State
import Data.List

type Coins = [Int]

type StMach a = StateT Coins IO a

main :: IO ()
main = 
  let 
    coins = [1,5,10,25]
  in
    do 
      putStrLn "Enter an amount: "
      input <- getLine
      make_change coins (read input)

make_change :: Coins -> Int -> IO () -- IO (Int, Coins) 
make_change coins amt = runStateT (recurs amt) coins *> return ()

-- author: Monad Book
recurs :: Int -> StMach Int
recurs amt = 
	if amt == 0
		then return amt
		else (next_coin amt >>= recurs)	

--if amount is less than (max coins), then delete the max from coins
--otherwise subtract amount and delete that elem from coins (unless it's 1)
next_coin :: Int -> StMach Int
next_coin amt = do 
	innerS <- get
	lift $ putStrLn $ "(" ++ (show amt) ++ ", " ++ (show innerS) ++ ")"	
	s      <- state $ \coins -> 
		 	case maxChangeCoin coins amt of
				Just m  -> (amt - m, if (m == 1) then coins else delete m coins)
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

removeMaxCoin :: (Num a, Ord a) => [a] -> [a]
removeMaxCoin []  = []
removeMaxCoin [1] = [1]
removeMaxCoin xs  = delete (maximum xs) xs

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