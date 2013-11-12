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
e3s7_oska board player_char depth
  | player == W = fst (e3s7_getBestNextMove board player depth)
  | otherwise   = reverse (fst (e3s7_getBestNextMove (reverse board) player depth)) 
  where player  = (e3s7_charToPlayer player_char)

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
    | p == B    = W
    | otherwise = E

e3s7_getBestNextMove :: [String] -> Player -> Int -> ([String], Int)
{-
    finds the most optimal move for a given number of steps to search ahead
    using the minimax strategy

    TODO: rewrite this so it considers the scores of moves further ahead 
          than the immediate next step
-}
e3s7_getBestNextMove board player depth 
    | depth == 0 = (head (e3s7_generateNewMoves board player), 0) -- stops tricky TAs from breaking the program
    | depth == 1 = (bestMove [(i, e3s7_evaluateBoard i player) | i <- (e3s7_generateNewMoves board player)])
    | otherwise  = e3s7_getBestNextMove someboard player (depth -1) 
    where bestMove boards = (head (reverse (sortBy e3s7_compareBoards boards)))
          someboard = (fst (e3s7_getOpponentBestNextMove (fst (bestMove [(i, e3s7_evaluateBoard i player) | i <- (e3s7_generateNewMoves board player)])) player))
    -- | otherwise  = last (sortBy e3s7_compareBoards possibleMoves)
    --    where possibleMoves = [e3s7_getBestNextMove i player (depth - 1) | i <- e3s7_generateNewMoves (e3s7_getOpponentBestNextMove board player) player]

e3s7_getOpponentBestNextMove :: [String] ->  Player -> ([String], Int)
{-
    returns the opponent's next best move given a board 

    we assume the opponent chooses the best NEXT move, looking one step ahead
-}
e3s7_getOpponentBestNextMove board player = (reverse (fst move), snd move)
  where move = (e3s7_getBestNextMove (reverse board) (e3s7_otherPlayer player) 1)

e3s7_compareBoards :: ([String], Int) -> ([String], Int) -> Ordering
e3s7_compareBoards a b = compare (snd a) (snd b)

e3s7_evaluateBoard :: [String] -> Player -> Int
{-
   returns a board's score based on its potential for a win or loss 
   - a higher score is better
   
   scoring algorithm:
   score = (our piece count - their piece count) 
         + (n - our closest distance to their side) 
         - (n - their closest distance to our side)

   we treat 'W' as our player, because it simplifies the code.
   in order to fetch the evaluation for 'B', just flip the sign
   this works because the scores are symmetrical

   any heuristics we write will be placed here as well
-} 
e3s7_evaluateBoard board player
  | e3s7_distanceToEdge board player == 0 || null theirs = length board
  | e3s7_distanceToEdge board (e3s7_otherPlayer player) == 0 || null ours = length board
  | otherwise                                   = (length ours) 
                                                - (length theirs)
                                                + (length board - e3s7_distanceToEdge board player)
                                                - (length board - e3s7_distanceToEdge (reverse board) (e3s7_otherPlayer player))
  where ours = e3s7_findPlayersPieces board player;
        theirs = e3s7_findPlayersPieces board (e3s7_otherPlayer player);

e3s7_distanceToEdge :: [String] -> Player -> Int        
e3s7_distanceToEdge board player 
  | player == W = (length board - 1) - fst (snd (maximumBy (distanceTo) (e3s7_findPlayersPieces board player)))
  | otherwise   = (length board - 1) - fst (snd (maximumBy (distanceTo) (e3s7_findPlayersPieces (reverse board) player)))
  where distanceTo p1 p2 = compare (fst (snd p1)) (fst (snd p2))

e3s7_generateNewMoves :: [String] -> Player -> [[String]]
{-
   given a board, generate all possible valid next moves for a player
-}
e3s7_generateNewMoves board player = e3s7_generateForwardMoves board player pieces [] ++ e3s7_generateJumpMoves board player pieces []
                                where pieces = e3s7_findPlayersPieces board player

e3s7_moveLeftForward :: [String] -> (Int, Int) -> (Int, Int)
e3s7_moveLeftForward board pos
  -- no need for `floor`, `div` truncates the decimal
  | fst pos < (length board) `div` 2  = (fst pos + 1, snd pos - 1)
  | fst pos >= (length board) `div` 2 = (fst pos + 1, snd pos)

e3s7_moveRightForward :: [String] -> (Int, Int) -> (Int, Int)
e3s7_moveRightForward board pos
  | fst pos < (length board) `div` 2 = (fst pos + 1, snd pos)
  | fst pos >= (length board) `div` 2 = (fst pos + 1, snd pos + 1)

e3s7_isValidPosition :: [String] -> (Int, Int) -> Bool
e3s7_isValidPosition board pos
  | fst pos < 0 || fst pos >= length board              = False
  | snd pos < 0 || snd pos >= length (board !! fst pos) = False
  | otherwise                                           = True

e3s7_getPieceAheadLeft :: [String] -> Piece -> Piece
e3s7_getPieceAheadLeft board p 
  | e3s7_isValidPosition board forwardLeft = (e3s7_charToPlayer (board !! (fst forwardLeft) !! (snd forwardLeft)), forwardLeft)
  | otherwise                              = (E, forwardLeft)
  where forwardLeft = (e3s7_moveLeftForward board (snd p))

e3s7_getPieceAheadRight :: [String] -> Piece -> Piece
e3s7_getPieceAheadRight board p
  | e3s7_isValidPosition board forwardRight = (e3s7_charToPlayer (board !! (fst forwardRight) !! (snd forwardRight)), forwardRight)
  | otherwise                               = (E, forwardRight)
  where forwardRight = (e3s7_moveRightForward board (snd p))

e3s7_isValidMove :: [String] -> Piece -> Bool
e3s7_isValidMove board piece = not (stackedPieces (e3s7_findPieces board)) && e3s7_isValidPosition board (snd piece)
        where stackedPieces pieces 
                | null pieces                    = False
                | snd (head pieces) == snd piece = True
                | otherwise                      = stackedPieces (tail pieces);

e3s7_isValidJump :: [String] -> Piece -> Piece -> Bool
e3s7_isValidJump board jumper jumpee
  | e3s7_getPieceAheadLeft board jumper == jumpee 
    && fst jumper == e3s7_otherPlayer (fst jumpee)
    && e3s7_isValidMove board (fst jumper, e3s7_moveLeftForward board (snd jumpee))  = True
  | e3s7_getPieceAheadRight board jumper == jumpee 
    && fst jumper == e3s7_otherPlayer (fst jumpee)
    && e3s7_isValidMove board (fst jumper, e3s7_moveRightForward board (snd jumpee)) = True
  | otherwise                                                                        = False

e3s7_generateForwardMoves :: [String] -> Player -> [Piece] -> [Piece] -> [[String]]
e3s7_generateForwardMoves board player pieces used_pieces
    | null pieces = []
    | otherwise   = e3s7_forwardMovesForPiece board (pieces ++ used_pieces) ++ e3s7_generateForwardMoves board player (tail pieces) (head pieces:used_pieces)

e3s7_trimEmptyLists :: [[a]] -> [[a]]
e3s7_trimEmptyLists lists 
  | null lists        = []
  | null (head lists) = e3s7_trimEmptyLists (tail lists)
  | otherwise         = head lists:e3s7_trimEmptyLists (tail lists)

e3s7_forwardMovesForPiece :: [String] -> [Piece] -> [[String]]
e3s7_forwardMovesForPiece board pieces = e3s7_trimEmptyLists (getLeftMoves:[getRightMoves])
                                    where getLeftMoves 
                                            | e3s7_isValidMove board leftForward  = e3s7_drawBoard board (leftForward:rest_pieces)
                                            | otherwise                           = [];
                                          getRightMoves
                                            | e3s7_isValidMove board rightForward = e3s7_drawBoard board (rightForward:rest_pieces)
                                            | otherwise                           = [];
                                          leftForward = (fst piece, e3s7_moveLeftForward board (snd piece));
                                          rightForward = (fst piece, e3s7_moveRightForward board (snd piece));
                                          piece = head pieces;
                                          rest_pieces = tail pieces ++ e3s7_findPlayersPieces board (e3s7_otherPlayer (fst piece))

e3s7_generateJumpMoves :: [String] -> Player -> [Piece] -> [Piece] -> [[String]]
e3s7_generateJumpMoves board player pieces used_pieces
  | null pieces = []
  | otherwise   = e3s7_jumpMovesForPiece board (pieces ++ used_pieces) ++ e3s7_generateJumpMoves board player (tail pieces) (head pieces:used_pieces)


e3s7_jumpMovesForPiece :: [String] -> [Piece] -> [[String]]
e3s7_jumpMovesForPiece board pieces = e3s7_trimEmptyLists (getLeftJumps:[getRightJumps])
  where getLeftJumps 
          | e3s7_isValidJump board piece leftJump = e3s7_drawBoard board (leftJumpLanding:rest_pieces leftJump)
          | otherwise                             = [];
        getRightJumps
          | e3s7_isValidJump board piece rightJump = e3s7_drawBoard board (rightJumpLanding:rest_pieces rightJump)
          | otherwise                             = [];
        piece = head pieces;
        leftJump = e3s7_getPieceAheadLeft board piece;
        leftJumpLanding = (fst piece, e3s7_moveLeftForward board (snd leftJump))
        rightJump = e3s7_getPieceAheadRight board piece;
        rightJumpLanding = (fst piece, e3s7_moveRightForward board (snd rightJump))
        rest_pieces jumped = tail pieces ++ removeJumpedPiece (e3s7_findPlayersPieces board (e3s7_otherPlayer (fst piece))) jumped
        removeJumpedPiece ps jumped
          | null ps           = []
          | head ps == jumped = removeJumpedPiece (tail ps) jumped
          | otherwise         = head ps:removeJumpedPiece (tail ps) jumped

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
