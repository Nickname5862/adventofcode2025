module Utils where

solve :: String -> Int
solve _ = 2




{--

Useful functions:

- map, filter, length
- read, show
- lines, words, length
- all, any/some/idk, not, null
- take, drop
- transpose
- groupBy
- find, loopup
- concat, concatMap
- zipWith
- catMaybes
- <$>, <*>
- maybe
- foldr, scanr, 
- trace, traceShow
- zip, intercalate

--}




-- get a line, take x, `read`-parse it to the right type, and put it all in a list
-- harder than it seems...
-- splitLine :: Read a => String -> [Int] -> [a] -> [a]






-- In Haskell, I want the following:
-- Given an input string, e.g. "3 abc def",
-- I want to make a parses which gets a:
-- - string (the input string);
-- - list of int (the number of characters to parse, here [1, 1, 3, 1, 3] for the number, space, letters, space, letters);
-- - a list of types to parse them to (here, [Int, String, String, String], all instance to `Read`);
-- Which then uses `take` to take parts of the input string, parse them to the type, and output the 