module Main where

type Board = [Int]
type Move = (Int, Int) -- wiersz, ile

initialBoard :: Board
initialBoard = [5,4,3,2,1]

printRow :: Int -> String
printRow n = concat $ replicate n "* "

showBoard :: Board -> String
showBoard board = aux board 0 where
    aux [] _     = ""
    aux (x:xs) n = (show n) ++ ": " ++ printRow x ++ "\n" ++ aux xs (n + 1)

isCorrectMove :: Board -> Move -> Bool
isCorrectMove board (n, m) = n >= 0 && n < length board && m > 0 && m <= (board !! n)

readM :: Read a => String -> Maybe a
readM str = case reads str of
    []      -> Nothing
    (n,_):_ -> Just n

makeMove :: Board -> Move -> Board
makeMove board (n,m) = filter (/=0) $ map (\(i,x) -> if i == n then x - m else x) $ zip [0..] board

aiMove :: Board -> Move
aiMove (x:_) = (0,x)

getMove :: IO (Maybe Move)
getMove = do
    putStr "Podaj nr stosu: "
    nStr <- getLine
    putStr "Ile kamyków zabrać: "
    mStr <- getLine
    let maybeN = readM nStr
    let maybeM = readM mStr
    case (maybeN, maybeM) of
        (Just n, Just m) -> return $ Just (n, m)
        _                -> return Nothing

getCorrectMove :: Board -> IO Move
getCorrectMove board = do
    mn <- getMove
    case mn of
        Nothing -> getCorrectMove board
        Just ans  -> if isCorrectMove board ans then return ans else putStrLn "Raz jeszcze!" >> getCorrectMove board

gameLoop :: Board -> IO ()
gameLoop board = do
    putStrLn (showBoard board)
    move <- getCorrectMove board
    let board' = makeMove board move
    case board' of
        [] -> putStrLn "Gracz wygrywa!"
        _  -> let board'' = makeMove board' (aiMove board') in
                case board'' of
                    [] -> putStrLn "Komp wygrywa!"
                    _  -> gameLoop board''

main :: IO ()
main = gameLoop initialBoard
