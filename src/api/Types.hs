{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import qualified Data.Text as T
import           Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.ByteString.Char8 as B
import ChessBoard
import ChessBoard.Position
import Text.Read

newtype JsonBoard = JsonBoard { getBoard :: Board }
instance ToJSON JsonBoard where
  toJSON json_board = do
    let boardList = boardToList $ getBoard json_board
        encodeCoord x y = T.pack $ [("ABCDEFGH" !! y)] ++ (show $ x+1)
        encodeSquare x y = do
          encodeCoord x y .= case (boardList !! x) !! y of
            Just piece -> show piece
            Nothing -> ""
    object [ encodeSquare x y | x <- [0..7], y <- [0..7] ]

newtype JsonMoveRecord = JsonMoveRecord { getMoveRecord :: MoveRecord }
instance ToJSON JsonMoveRecord where
  toJSON json_mr = do
    let (piece_num, from_mr, to_mr) = getMoveRecord json_mr
    let (from_pos, to_pos) = (encodePosition from_mr, encodePosition to_mr)
    object [ "piece_num" .= piece_num, "from" .= from_pos, "to" .= to_pos ]

newtype JsonMoveRecords = JsonMoveRecords { getMoveRecords :: MoveRecords }
instance ToJSON JsonMoveRecords where
  toJSON json_mrs = do
    let zipped_records = zip ([1..] :: [Integer]) $ reverse $ getMoveRecords json_mrs
    object [ ((T.pack $ show moveNum) .= JsonMoveRecord mr) | (moveNum, mr) <- zipped_records ]

encodePosition :: Position -> String
encodePosition pos = [("ABCDEFGH" !! y)] ++ (show $ x + 1)
      where (x, y) = posToCoord pos

decodePosition :: B.ByteString -> Maybe Position
decodePosition str = do
  if B.length str == 2 then do
    let x_str = [B.last str] :: String
        y_str = B.head str
    case (readMaybe x_str :: Maybe Int) of
      Just x ->
        case B.elemIndex y_str "ABCDEFGH" of
          Just y -> coordToPos (x - 1, y)
          Nothing -> error "Invalid Y coordinate passed"
      Nothing -> error "Invalid X coordinate passed"
  else error "Invalid board coordinate passed"
