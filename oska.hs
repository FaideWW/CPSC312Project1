-- CPSC 312 Project 1
-- Michael Ko (31477102)
-- Saewon Kye (14680110)
-- Oct 29, 2013
import Data.List

data Player = W 
            | B 
            | E -- empty space
              deriving (Eq, Show)

type Piece = (Player, (Int, Int))

e3s7_oska :: [String] -> Char -> Int -> [String]
{- 
   front-facing interface function - returns the best move for a given player
   on a given turn and a given number of future moves to consider
-}
e3s7_oska board player_char depth = e3s7_getBestNextMove board (e3s7_charToPlayer player_char) depth

e3s7_charToPlayer :: Char -> Player
e3s7_charToPlayer p
    | p == 'w'  = W
    | p == 'b'  = B
    | otherwise = E

e3s7_playerToChar :: Player -> Char
e3s7_playerToChar p
    | p == W    = 'w'
    | p == B    = 'b'
    | otherwise = '-'

e3s7_otherPlayer :: Player -> Player
e3s7_otherPlayer p
    | p == W    = B
    | otherwise = W

e3s7_getBestNextMove :: [String] -> Player -> Int -> [String]
{-
    finds the most optimal move for a given number of steps to search ahead
    using the minimax strategy

    TODO: rewrite this so it considers the scores of moves further ahead 
          than the immediate next step
-}
e3s7_getBestNextMove board player depth 
    | depth == 0 = head (e3s7_generateNewMoves board player) -- stops tricky TAs from breaking the program
    | depth == 1 = last (sortBy e3s7_compareBoards (e3s7_generateNewMoves board player))
    | otherwise  = last (sortBy e3s7_compareBoards possibleMoves)
        where possibleMoves = [e3s7_getBestNextMove i player (depth - 1) | i <- e3s7_generateNewMoves (e3s7_getOpponentBestNextMove board player) player]

e3s7_getOpponentBestNextMove :: [String] ->  Player -> [String]
{-
    returns the opponent's next best move given a board 

    TODO: clarify whether we assume the opponent is looking ahead as far as 
          we are
-}
e3s7_getOpponentBestNextMove board player = e3s7_getBestNextMove (reverse board) (e3s7_otherPlayer player) 1

e3s7_compareBoards :: [String] -> [String] -> Ordering
-- sorts boards by their evaluated score
e3s7_compareBoards a b = compare (e3s7_evaluateBoard a) (e3s7_evaluateBoard b)

e3s7_evaluateBoard :: [String] -> Int
{-
   returns a board's score based on its potential for a win or loss 
   - a higher score is better
   
   scoring algorithm:
   score = (our piece count - their piece count) 
         + (n - our closest distance to their side) 
         - (n - their closest distance to our side)

   any heuristics we write will be placed here as well
-} 
e3s7_evaluateBoard board = undefined


e3s7_generateNewMoves :: [String] -> Player -> [[String]]
{-
   given a board, generate all possible valid next moves for a player
-}
e3s7_generateNewMoves board player = e3s7_generateForwardMoves board player pieces ++ e3s7_generateJumpMoves board player pieces
                                where pieces = e3s7_findPlayersPieces board player

e3s7_isValidMove :: [String] -> Piece -> Bool
e3s7_isValidMove board piece = not (stackedPieces (e3s7_findPieces board) || outOfBounds)
        where stackedPieces pieces 
                | null pieces                    = False
                | snd (head pieces) == snd piece = True
                | otherwise                      = stackedPieces (tail pieces);
              outOfBounds 
                | fst (snd piece) < 0 || fst (snd piece) > length board                      = True
                | snd (snd piece) < 0 || snd (snd piece) > length (board !! fst (snd piece)) = True
                | otherwise                                                                  = False

e3s7_generateForwardMoves :: [String] -> Player -> [Piece] -> [[String]]
e3s7_generateForwardMoves board player pieces 
    | null pieces = []
    | otherwise   = e3s7_forwardMovesForPiece board pieces ++ e3s7_generateForwardMoves board player (tail pieces)

e3s7_forwardMovesForPiece :: [String] -> [Piece] -> [[String]]
e3s7_forwardMovesForPiece board pieces = trimEmptyLists (getLeftMoves:[getRightMoves])
                                    where getLeftMoves 
                                            | e3s7_isValidMove board leftForward  = e3s7_drawBoard board (leftForward:tail pieces)
                                            | otherwise                           = [];
                                          getRightMoves
                                            | e3s7_isValidMove board rightForward = e3s7_drawBoard board (rightForward:tail pieces)
                                            | otherwise                           = [];
                                          leftForward 
                                            -- no need for `floor`: `div` truncates the decimal
                                            | fst (snd piece) < (length board) `div` 2  = (fst piece, (fst (snd piece) + 1, snd (snd piece) - 1))
                                            | fst (snd piece) >= (length board) `div` 2   = (fst piece, (fst (snd piece) + 1, snd (snd piece)));
                                          rightForward
                                            | fst (snd piece) < (length board) `div` 2  = (fst piece, (fst (snd piece) + 1, snd (snd piece)))
                                            | fst (snd piece) >= (length board) `div` 2   = (fst piece, (fst (snd piece) + 1, snd (snd piece) + 1));
                                          piece = head pieces;
                                          trimEmptyLists lists 
                                            | null lists        = []
                                            | null (head lists) = trimEmptyLists (tail lists)
                                            | otherwise         = head lists:trimEmptyLists (tail lists)

e3s7_generateJumpMoves :: [String] -> Player -> [Piece] -> [[String]]
e3s7_generateJumpMoves board player pieces = undefined

e3s7_drawBoard ::  [String] -> [Piece] -> [String]
{-
    drawBoard
    for every space on a board the same size as a reference board:
        if the space is occupied by a piece, fill it
        otherwise put '-'

    Passing in another board for reference is not elegant at all but since the
     board size is dynamic, we need to retrieve the size in order to draw it 
-}
e3s7_drawBoard example pieces = drawBoardHelper (e3s7_sortPieces pieces) example 0
    where drawBoardHelper pieces example row 
            | row == length example = []
            | otherwise                = drawRow (row, 0) pieces:drawBoardHelper pieces example (row + 1);
          drawRow position pieces 
            | snd position == length (example !! (fst position)) = []
            | otherwise                                          = charAt position pieces:drawRow (fst position, (snd position) + 1) pieces;
          charAt position pieces
            | null pieces = '-'
            | snd (head pieces) == position = e3s7_playerToChar (fst (head pieces))
            | otherwise = charAt position (tail pieces)



e3s7_sortPieces :: [Piece] -> [Piece]
--sort pieces by position
e3s7_sortPieces pieces = sortBy pieceSort pieces
    where pieceSort p1 p2 
            | fst (snd p1) <  fst (snd p2) = LT
            | fst (snd p1) >  fst (snd p2) = GT
            | fst (snd p1) == fst (snd p2) = compare (snd (snd p1)) (snd (snd p2))

e3s7_findPlayersPieces :: [String] -> Player -> [Piece] 
{-
   find all of a player's pieces by filtering the results of e3s7_findPieces
-}
e3s7_findPlayersPieces board player = filter byPlayer (e3s7_findPieces board)
                                    where byPlayer x = player == fst x



e3s7_findPieces :: [String] -> [Piece]
{-
   given a board, find all pieces and their positions
-}
e3s7_findPieces board = e3s7_findPiecesHelper board 0

e3s7_findPiecesHelper :: [String] -> Int -> [Piece]
e3s7_findPiecesHelper board row
    | null board = []
    | otherwise  = piecesInRow (head board) row 0 ++ e3s7_findPiecesHelper (tail board) (row + 1)
        where piecesInRow row row_num col_num
                | null row                          = []
                | e3s7_charToPlayer (head row) /= E = (e3s7_charToPlayer (head row), (row_num, col_num)):piecesInRow (tail row) row_num (col_num + 1)
                | otherwise                         = piecesInRow (tail row) row_num (col_num + 1)
