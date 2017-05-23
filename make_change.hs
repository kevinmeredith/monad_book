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
      input  <- getLine
      number <- return $ read input 
      make_change coins number

make_change :: Coins -> Int -> IO () -- IO (Int, Coins) 
make_change coins amt = runStateT (recurs amt) coins *> return ()

-- author: Monad Book
recurs :: Int -> StMach Int
recurs amt = 
	if amt == 0
		then return amt
		else next_coin amt >>= recurs

data CoinState a = CoinLargerThanAmount a | FoundChange a | PennyMatch

--if amount is less than (max coins), then delete the max from coins
--otherwise subtract amount and delete that elem from coins (unless it's 1)
next_coin :: Int -> StMach Int
next_coin amt = do 
  coinsLeft <- get
  dispense amt coinsLeft
  state $ \coins -> 
		 	case coinState coins amt of
        PennyMatch             -> (amt-1, coins)
        FoundChange a          -> (amt-a, delete a coins)
        CoinLargerThanAmount a -> (amt,   delete a coins)

coinState :: Ord a => [a] -> a -> CoinState a
coinState xs a =  
 let 
  largerCoinInChange = find (> a) xs
 in 
  case largerCoinInChange of 
    Just a  -> CoinLargerThanAmount a
    Nothing -> case  filter (<= a) xs of
     	[] -> PennyMatch
     	as -> FoundChange (maximum as)

removeMaxCoin :: (Num a, Ord a) => [a] -> [a]
removeMaxCoin []  = []
removeMaxCoin [1] = [1]
removeMaxCoin xs  = delete (maximum xs) xs

-- author: Monad Book
dispense :: Int -> Coins -> StMach ()
dispense i coins = 
   ( lift $ 
      putStrLn ( "dispense "
                  ++(show i)
                  ++" cents"
                  ++ remaining)
    )
   where 
     cents = (if i==1  
                then " cent" 
                else " cents") ++ remaining
     remaining = " with remaining coins: " ++ (show coins)

-- credit for help: http://stackoverflow.com/a/44029311/409976
f :: Int -> StateT [Int] IO Int
f i = state $ \xs -> case xs of
	[]     -> (i, [])
	(a:as) -> (i+a, as)

update :: Int -> [Int] -> (Int, [Int])      
update x []     = (x, [])
update x (y:ys) = (x+y, ys)        