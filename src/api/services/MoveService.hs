{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api.Services.MoveService where

import Api.Types
import ChessBoard
import Snap.Core
import Snap.Snaplet
import Control.Lens
import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens (_String, key)
import Network.Wreq
import Api.Services.BoardService
import ChessBoard
import ChessBoard.Pieces
import ChessBoard.Position
import Data.List

type PossibleMove = Coord
data BoardContext = BoardContext { __board :: Board, __move_records :: MoveRecords }

data MoveState = MoveState { _moves :: MoveRecords, _counter :: Int }
makeLenses ''MoveState

initializeMoveState :: MoveState
initializeMoveState = MoveState { _moves = [], _counter = 0 }

data MoveService = MoveService { _moveStateVar :: MVar MoveState
                               , _boardService :: Snaplet BoardService }
makeLenses ''MoveService

type MoveServiceHandler b = Handler b MoveService ()

moveRoutes :: [(B.ByteString, MoveServiceHandler b)]
moveRoutes = [ ("", method GET listMoves)
             , ("", method POST doMove)
             , ("available", method GET listAvailableMoves)
             , ("do", method GET doMove) ]

readMoves :: MVar MoveState -> IO MoveRecords
readMoves ms_var = do
  current_ms <- readMVar ms_var
  return $ _moves current_ms

listMoves :: MoveServiceHandler b
listMoves = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  current_move_list <- liftIO . readMoves =<< view moveStateVar
  writeLBS $ BL.pack $ "{\"status\": \"ok\", \"moves\": " ++ (BL.unpack . encode $ JsonMoveRecords current_move_list) ++ "}"

rangeBetweenCoords :: Coord -> Coord -> ([Int], [Int])
rangeBetweenCoords (from_x, from_y) (to_x, to_y) = (xs, ys) where
  xs = [from_x,(from_x + (if from_x > to_x then -1 else 1))..to_x]
  ys = [from_y,(from_y + (if from_y > to_y then -1 else 1))..to_y]

shortestLineBetween :: Coord -> Coord -> [Coord]
shortestLineBetween from_coord to_coord
  | from_x == to_x = [(x, y) | x <- xs, y <- ys, to_x - x == 0]
  | from_y == to_y = [(x, y) | x <- xs, y <- ys, to_y - y == 0]
  | (abs $ to_x - from_x) == (abs $ to_y - from_y) = [(x, y) | x <- xs, y <- ys, (abs $ to_x - x) == (abs $ to_y - y)]
  | otherwise = []
  where
  ((from_x, from_y), (to_x, to_y)) = (from_coord, to_coord)
  (xs, ys) = rangeBetweenCoords (from_x, from_y) (to_x, to_y)

positionsBetweenCoords :: Coord -> Coord -> [Position]
positionsBetweenCoords from_coord to_coord =
  [p | Just p <- possible_positions_between] where
  shortest_line = shortestLineBetween from_coord to_coord
  possible_positions_between = [coordToPos c | c <- shortest_line, c /= from_coord, c /= to_coord]

firstOccupiedSpaceBetween :: Coord -> Coord -> Board -> Maybe Position
firstOccupiedSpaceBetween from_coord to_coord current_board =
  find position_is_occupied $ positionsBetweenCoords from_coord to_coord where
  position_is_occupied pos = pieceAt current_board pos /= Nothing

relativeMovementsForPiece :: Piece -> [PossibleMove]
relativeMovementsForPiece piece =
  case piece of
    Pawn ->   []
    Knight -> [ ( 2, 1 ), ( 2, -1 )
              , ( 1, 2 ), ( 1, -2 )
              , (-1, 2 ), (-1, -2 )
              , (-2, 1 ), (-2, -1 ) ]
    Rook ->   [ (nx, ny) | nx <- [-7..7], ny <- [-7..7], (nx, ny) /= (0, 0),   nx == 0 || ny == 0   ]
    Bishop -> [ (nx, ny) | nx <- [-7..7], ny <- [-7..7], (nx, ny) /= (0, 0), (abs nx) == (abs ny) ]
    Queen ->  [ (nx, ny) | nx <- [-7..7], ny <- [-7..7], (nx, ny) /= (0, 0), (abs nx) == (abs ny) || nx == 0 || ny == 0 ]
    King ->   [ (nx, ny) | nx <- [-1..1], ny <- [-1..1], (nx, ny) /= (0, 0) ]

movementMaximumsForPiece :: Piece -> [PossibleMove]
movementMaximumsForPiece piece = do
  case piece of
    Rook   -> [ (  0, -7 ), (  0, 7 ), ( 7,  0 ), ( -7, 0 ) ]
    Bishop -> [ ( -7, -7 ), ( -7, 7 ), ( 7, -7 ), (  7, 7 ) ]
    Queen  -> [ ( -7, -7 ), ( -7, 7 ), ( 7, -7 ), (  7, 7 )
              , (  0, -7 ), (  0, 7 ), ( 7,  0 ), ( -7, 0 ) ]
    _ -> relativeMovementsForPiece piece

relativeSpecialMovementsForPiece :: Piece -> Position -> [(PossibleMove, BoardContext -> Bool)]
relativeSpecialMovementsForPiece piece current_pos =
  case piece of
    Pawn ->   [ (( 1,  0 ), pawnCanMoveOneStep    current_pos)
              , (( 2,  0 ), pawnCanMoveTwoSteps   current_pos)
              , (( 1,  1 ), pawnCanCapturePieceAt current_pos ( 1,  1 ))
              , (( 1, -1 ), pawnCanCapturePieceAt current_pos ( 1, -1 )) ]
    Rook   -> [] -- TODO: Castling, both sides
    King   -> [] -- TODO: Castling, both sides
    _ -> []

pawnCanMoveOneStep :: Position -> BoardContext -> Bool
pawnCanMoveOneStep pos board_context = True

pawnCanMoveTwoSteps :: Position -> BoardContext -> Bool
pawnCanMoveTwoSteps pos board_context = pieceHasNotMoved pos board_context

pieceHasMoved :: Position -> BoardContext -> Bool
pieceHasMoved pos board_context = findMoveRecordByPiece pos board_context /= Nothing

findMoveRecordFrom :: Position -> MoveRecords -> Maybe MoveRecord
findMoveRecordFrom _ [] = Nothing
findMoveRecordFrom pos (mr:mrs)
  | from_pos == pos = Just mr
  | otherwise = findMoveRecordFrom pos mrs
  where
  (_, from_pos, _) = mr

findMoveRecordByPiece :: Position -> BoardContext -> Maybe MoveRecord
findMoveRecordByPiece _ (BoardContext _ []) = Nothing
findMoveRecordByPiece pos (BoardContext current_board move_records) =
  case pieceAt current_board pos of
    Nothing -> Nothing
    Just (PlayerPiece piece_id _ _) ->
      trace_move_records_for_piece_id piece_id move_records
  where
  trace_move_records_for_piece_id _piece_id [] = Nothing
  trace_move_records_for_piece_id _piece_id (mr:mrs) = do
    let (mr_piece_id, _, _) = mr
    if mr_piece_id == _piece_id then Just mr
    else trace_move_records_for_piece_id _piece_id mrs

pieceHasNotMoved :: Position -> BoardContext -> Bool
pieceHasNotMoved pos context = not $ pieceHasMoved pos context

applyPossibleMove :: Coord -> PossibleMove -> Coord
applyPossibleMove (x, y) (x_movement, y_movement) = (x + x_movement, y + y_movement)

applyPossibleMoveToPosition :: Position -> PossibleMove -> Maybe Position
applyPossibleMoveToPosition pos movement =
  coordToPos $ applyPossibleMove (posToCoord pos) movement

pawnCanCapturePieceAt :: Position -> PossibleMove -> BoardContext -> Bool
pawnCanCapturePieceAt pawn_pos movement (BoardContext current_board _) = do
  case applyPossibleMoveToPosition pawn_pos movement of
    Nothing -> False
    Just dest_pos -> do
      case pieceAt current_board dest_pos of
        Nothing -> False
        Just (PlayerPiece _ occupying_player _) ->
          case pieceAt current_board pawn_pos of
            Just (PlayerPiece _ current_player Pawn) ->
              current_player /= occupying_player
            _ -> False

getAvailableMoves :: Board -> MoveRecords -> Position -> PlayerPiece -> MoveRecords
getAvailableMoves current_board move_records pos (PlayerPiece piece_num player piece) =
  normal_moves ++ special_moves where
  (x, y) = posToCoord pos
  apply_movement = \(x_move, y_move) -> do
    if piece == Knight then
      case coordToPos (x + x_move, y + y_move) of
        Nothing -> []
        Just p ->
          case playerAtPosition current_board p of
            Nothing -> [(piece_num, pos, p)]
            Just occupying_player ->
              if player == occupying_player then [] else [(piece_num, pos, p)]
    else do
      let move_coord_vector = shortestLineBetween (x, y) (x + x_move, y + y_move)
          move_pos_vector = [p | Just p <- [coordToPos c | c <- move_coord_vector], p /= pos]
          move_vector = unblockedMoves current_board player move_pos_vector
      [(piece_num, pos, p) | p <- move_vector]
  apply_movements = \acc relative_move -> (apply_movement relative_move) ++ acc
  apply_special_movements = \acc ((x_move, y_move), can_move_fn) -> do
    if can_move_fn (BoardContext current_board move_records) then
      (apply_movement (x_move, y_move)) ++ acc
    else acc
  normal_moves  = foldl apply_movements [] $ movementMaximumsForPiece piece
  special_moves = foldl apply_special_movements [] $ relativeSpecialMovementsForPiece piece pos

unblockedMoves :: Board -> Player -> [Position] -> [Position]
unblockedMoves current_board current_player movement_vector =
  case movement_vector of
    [] -> []
    (m:ms) ->
      case playerAtPosition current_board m of
        Nothing -> m : unblockedMoves current_board current_player ms
        Just occupying_player ->
          if current_player == occupying_player then [] else [m]

playerAtPosition :: Board -> Position -> Maybe Player
playerAtPosition current_board pos =
  case pieceAt current_board pos of
    Nothing -> Nothing
    Just (PlayerPiece _ occupying_player _) -> Just occupying_player

listAvailableMoves :: MoveServiceHandler b
listAvailableMoves = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  maybeFromParam <- getParam "from"
  case maybeFromParam of
    Just fromParam -> do
      current_board <- with boardService $ do
        gs_var <- view gameStateVar
        liftIO $ readBoard gs_var
      liftIO . putStrLn $ show current_board
      case decodePosition fromParam of
        Just fromPos -> do
          case pieceAt current_board fromPos of
            Just piece -> do
              current_move_list <- liftIO . readMoves =<< view moveStateVar
              let available_moves_list = getAvailableMoves current_board current_move_list fromPos piece
              liftIO . putStrLn $ show piece
              -- liftIO . putStrLn . show $ coordToPos (0, 2)
              -- liftIO . putStrLn . show $ firstOccupiedSpaceBetween (0, 3) (3, 0) current_board
              writeLBS $ BL.pack $ "{\"status\": \"ok\", \"piece\": \"" ++ (show piece) ++ "\", \"moves\": " ++ (BL.unpack . encode $ JsonMoveRecords available_moves_list) ++ "}"
            Nothing ->
              writeLBS "{\"status\": \"not ok\", \"error\": \"There is no piece at the specified coordinate.\"}"
        Nothing ->
          writeLBS "{\"status\": \"not ok\", \"error\": \"The specified coordinate is invalid.\"}"
    Nothing ->
      writeLBS "{\"status\": \"not ok\", \"error\": \"You must include a from parameter.\"}"

requestFromBoardService :: String -> Options -> (Network.Wreq.Response BL.ByteString -> IO a) -> IO (Maybe String)
requestFromBoardService endpoint opts onsuccess = do
  r <- getWith opts $ "http://localhost:9000/api/board/" ++ endpoint
  case r ^? responseBody . key "status" . _String of
    Just "ok" -> do
      onsuccess r
      return Nothing
    Just other_status -> return $ Just $
      "The board service responded with status " ++ T.unpack other_status ++ ".\nResponse: " ++ (BL.unpack $ r ^. responseBody)
    Nothing -> return $ Just $
      "The board service did not respond comprehensibly.\nResponse: " ++ (BL.unpack $ r ^. responseBody)

putMove :: MoveRecord -> MVar MoveState -> IO (Maybe String)
putMove new_move ms_var = do
  let (_, raw_from_pos, raw_to_pos) = new_move
      [encoded_from_pos, encoded_to_pos] = T.pack . encodePosition <$> [raw_from_pos, raw_to_pos]
      opts = defaults & param "from" .~ [encoded_from_pos] & param "to" .~ [encoded_to_pos]
  r <- getWith opts "http://localhost:9000/api/board/modify"
  case r ^? responseBody . key "status" . _String of
    Just "ok" -> do
      current_ms <- takeMVar ms_var
      putMVar ms_var $! over moves ((:) new_move) current_ms
      return Nothing
    Just other_status -> return $ Just $
      "The board service responded with status " ++ T.unpack other_status ++ ".\nResponse: " ++ (BL.unpack $ r ^. responseBody)
    Nothing -> return $ Just $
      "The board service did not respond comprehensibly.\nResponse: " ++ (BL.unpack $ r ^. responseBody)

doMove :: MoveServiceHandler b
doMove = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  maybeFromParam <- getParam "from"
  maybeToParam <- getParam "to"
  case (maybeFromParam, maybeToParam) of
    (Just fromParam, Just toParam) -> do
      case (decodePosition fromParam, decodePosition toParam) of
        (Just fromPos, Just toPos) -> do
          current_board <- with boardService $ do
            gs_var <- view gameStateVar
            liftIO $ readBoard gs_var
          case pieceAt current_board fromPos of
            Just (PlayerPiece piece_num _ _) -> do
              let new_move = (piece_num, fromPos, toPos)
              maybe_err <- liftIO . putMove new_move =<< view moveStateVar
              case maybe_err of
                Nothing -> do
                  let json_move = BL.unpack . encode $ JsonMoveRecord new_move
                  writeLBS . BL.pack $ "{\"status\": \"ok\", \"move\": " ++ json_move ++ "}"
                Just err -> do
                  writeLBS . BL.pack $ "{\"status\": \"not ok\", \"error\": " ++ err ++ "}"
            Nothing -> writeLBS "{\"status\": \"not ok\", \"error\": \"There was no piece at the from position supplied.\"}"
        _ -> writeLBS "{\"status\": \"not ok\", \"error\": \"Failed to parse one of the positions supplied.\"}"
    _ -> writeLBS "{\"status\": \"not ok\", \"error\": \"You must include both a from and to parameter.\"}"

moveServiceInit :: Snaplet BoardService -> SnapletInit b MoveService
moveServiceInit boardService' = makeSnaplet "moves" "Move Service" Nothing $ do
  addRoutes moveRoutes
  let initial_ms = initializeMoveState
  ms_var <- liftIO $ do
    ms_var' <- newEmptyMVar
    putMVar ms_var' initial_ms
    return ms_var'
  return $ MoveService ms_var boardService'
