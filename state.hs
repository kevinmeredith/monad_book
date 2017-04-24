{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State

-- credit: book's solution
state_obj :: State [Char] [Char]
state_obj = state $ \s -> ("a", "b")

show_result :: Show a => State String a -> IO ()
show_result st = do
	(one, two) <- return $ runState st "anything"
	putStrLn $ "First: "  ++ (show one)
	putStrLn $ "Second: " ++ two


type Stack a = State [a]

processAsStack :: [a] -> Stack a () -> [a]
processAsStack xs st = snd $ runState st xs -- snd $ runState st []

printList lst = putStrLn (show lst)

push :: a -> Stack a ()
push a = state $ \(xs) -> ((), a:xs)

pop :: Stack a a 
pop = state $ \(x:xs) -> (x, xs)

doIt = printList $ processAsStack "ct" $ do
	x <- pop	
	push 'a'
	push x