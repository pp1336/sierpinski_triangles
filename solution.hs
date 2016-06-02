pass xss h 0 = reduce xss h h 1
pass (xs : xss) h hc = xs : (pass xss h (hc - 1))

reduce [] _ _ _ = []
reduce (xs : xss) h hc c | hc > 0 = (redcrow xs c c w) : (reduce xss h (hc - 1) (c + 2))
                         | otherwise = pass (xs : xss) h h
                           where
                             w = 2 * h - c


redcrow [] _ _ _ = []
redcrow (x : xs) c cc w | x == 0 = x: (redcrow xs c c w)
                        | cc == 0 = redcrow' (x : xs) w w c
                        | otherwise = x : (redcrow xs c (cc - 1) w)

redcrow' xs w 0 c = (take c xs) ++ (redcrow (drop c xs) c c w)
redcrow' (x : xs) w wc c = 0 : (redcrow' xs w (wc - 1) c)

getll = genll 1 []

genll n xss | n == 33 = xss
            | otherwise = (genl n) : (genll (n + 1) xss)
            
genl n = xs ++ ys ++ xs
       where
          y = 2 * n - 1
          x = div (63 - y) 2
          xs = replicate x 0
          ys = replicate y 1
          
fractal n = fractal' n 0 getll
fractal' n nc xss | n == nc = xss
                  | otherwise = fractal' n (nc + 1) (pass xss h h)
                     where
                       h = geth (nc + 1) 32
                    
geth nc z | nc == 0 = z
          | otherwise = geth (nc - 1) (div z 2)

printll (xs : []) = do
    printl xs
printll (xs : xss) = do
    printl xs
    printll xss
    
printl (x : []) | x == 0 = do
                             putStrLn "_"
                | otherwise = do
                                putStrLn (show x)
printl (x : xs) | x == 0 = do
                             putStr "_"
                             printl xs
                | otherwise = do
                                putStr (show x)
                                printl xs
    
main = do
    n <- getLine
    printll (fractal(read n))