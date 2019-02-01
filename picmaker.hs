data Pixel = Pixel { red :: Int
                   , grn :: Int
                   , blu :: Int
                   } deriving (Show)

main :: IO ()
main = do
    let firstgen = (replicate 499 False) ++ [True]
    --let firstgen = (replicate 374 False) ++ [True] ++ (replicate 375 False)
    --let firstgen = (take 500 . cycle) ([True, False, True, True])
    --let firstgen = take 500 . map ((<=2) . (`mod` 133)) $ fibs
    putStrLn $ header 500 500 255
    let gr = gradient 500
    let li = life firstgen
    putStrLn $ showBoard $ mask gr li

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

mask :: [[Pixel]] -> [[Bool]] -> [[Pixel]]
mask [] _ = []
mask _ [] = []
mask (x:xs) (y:ys) = (map jef (zipWith (,) x y)):(mask xs ys)
    where jef (p,m) = if m then p else (Pixel 0 0 0)

showPix :: Pixel -> String
showPix (Pixel r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

gradient :: (Enum a, RealFrac a) => a -> [[Pixel]]
gradient s = 
    [[(Pixel (round (r*(255.0/s))) (round (g *(255.0/s))) 255) | g <- [0..s]] | r <- [0..s]]

showBoard :: [[Pixel]] -> String
showBoard = unlines . map unwords . map (map showPix)

header :: Int -> Int -> Int -> String
header x y max_val =
    "P3 " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show max_val)

nextCell :: (Bool, Bool, Bool) -> Bool
nextCell (False, False, False) = False
nextCell (False, False, True ) = True
nextCell (False, True , False) = True
nextCell (False, True , True ) = True
nextCell (True , False, False) = False
nextCell (True , False, True ) = True
nextCell (True , True , False) = True
nextCell (True , True , True ) = False

--nextCell :: (Bool, Bool, Bool) -> Bool
--nextCell (False, False, False) = False
--nextCell (False, False, True ) = True
--nextCell (False, True , False) = True
--nextCell (False, True , True ) = True
--nextCell (True , False, False) = True
--nextCell (True , False, True ) = False
--nextCell (True , True , False) = False
--nextCell (True , True , True ) = False

--nextCell :: (Bool, Bool, Bool) -> Bool
--nextCell (False, False, False) = False
--nextCell (False, False, True ) = True
--nextCell (False, True , False) = False
--nextCell (False, True , True ) = True
--nextCell (True , False, False) = True
--nextCell (True , False, True ) = False
--nextCell (True , True , False) = True
--nextCell (True , True , True ) = False

nextGen :: [Bool] -> [Bool]
nextGen cells = map nextCell boys
    where   boys  = zipWith3 (,,) left cells right
            right = (tail cells) ++ [False]
            left  = (False:cells)

life :: [Bool] -> [[Bool]]
life cells = (cells:(life $ nextGen cells))
