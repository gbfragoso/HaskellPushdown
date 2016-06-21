-- Replace the top of stack (b) with content of (a)
replace :: [a] -> [a] -> [a]
replace a b = a ++ (drop 1 b)

-- Receive an pair (stack , word) and checks the existence of an production of type stack (top) -> word.
-- If true, return an tuple with result of operation
grammarRules :: [Char] -> [Char] -> ([Char],[Char])
grammarRules (x:xs) (y:ys)
    | x == y = (xs,ys)
    | x == 'E' && y == '0' = (replace ['0'] (x:xs),(y:ys))
    | x == 'E' && y == '1' = (replace ['1'] (x:xs),(y:ys))
    | x == 'E' && y == 'x' = (replace ['x'] (x:xs),(y:ys))
    | x == 'E' && y == 'y' = (replace ['y'] (x:xs),(y:ys))
    | x == 'E' && y == '(' = (replace ['(','E','A','E',')'] (x:xs),(y:ys))
    | x == 'A' && y == '+' = (replace ['+'] (x:xs),(y:ys))
    | x == 'A' && y == '-' = (replace ['-'] (x:xs),(y:ys))
    | x == 'A' && y == '*' = (replace ['*'] (x:xs),(y:ys))
    | x == 'A' && y == '/' = (replace ['/'] (x:xs),(y:ys))
    | otherwise = ([],y:ys)

-- Receive an pair (stack,word) and return True when reach ([],[])
pushdown :: ([Char],[Char]) -> Bool
pushdown ("","") = True
pushdown ("", _) = False
pushdown ((x:xs),(y:ys)) = pushdown(grammarRules (x:xs) (y:ys))

main :: IO()
main = do
    vet <- readFile "word.txt"
    let a = lines vet
    let word = head a
    let stack = ['E']
    print word
    print $ pushdown (stack,word)
