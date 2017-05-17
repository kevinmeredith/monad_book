import Control.Monad.State
import Data.List

type Coins = [Int]

type StMach a = StateT Coins IO a

make_change :: Coins -> Int -> IO (Int, Coins) -- IO ()
make_change coins amt = runStateT (recurs amt) coins

-- author: Monad Book
recurs :: Int -> StMach Int
recurs amt = 
	if amt == 0
		then return amt
		else next_coin amt >>= recurs

next_coin :: Int -> StMach Int
next_coin coin = state $ \coins -> 
				case maxChangeCoin coins coin of
					Just m  -> (5, delete m coins)
					Nothing -> (0, removeMaxCoin coins)

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

f :: StateT [Int] IO Int
f = state $ \xs -> update (error "I want int") xs

update :: Int -> [Int] -> (Int, [Int])      
update x []     = (x, [])
update x (y:ys) = (x+y, ys)        