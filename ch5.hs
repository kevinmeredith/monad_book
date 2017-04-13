f :: IO ()
f = getLine >>= 
       (\x -> 
           (getLine >>= 
               putStrLn >> 
                  return x)
           >>= putStrLn
       )           

-- g :: IO ()
-- g = getLine >> (getLine >>= putStrLn) >> putStrLn

-- h :: IO ()
-- h = getLine >>= (getLine >>= putStrLn) >>= putStrLn

