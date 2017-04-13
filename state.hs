import Control.Monad.State

-- credit: book's solution
state_obj :: State [Char] [Char]
state_obj = state $ \s -> ("a", "b")

show_result :: Show a => State String a -> IO ()
show_result st = do
	(one, two) <- return $ runState st
	putStrLn "First: one"
	putStrLn "Second: two"

