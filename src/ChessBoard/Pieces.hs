module ChessBoard.Pieces (
  Player (White,Black)
  , Piece (King,Queen,Bishop,Knight,Rook,Pawn)
  , PlayerPiece (PlayerPiece)
  , PieceNum
) where

-- | The types of player on the chess board.
data Player = White | Black deriving (Eq)

-- | All of the pieces that are on the board
data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq)

-- | Pieces on the chess board have both a player and a type
data PlayerPiece = PlayerPiece PieceNum Player Piece
instance Eq PlayerPiece where
  PlayerPiece aa ab ac == PlayerPiece ba bb bc =
    aa == ba && ab == bb && ac == bc

type PieceNum = Int

-- | Prints either "w" or "b"
instance Show Player where
  show White  = "w"
  show Black  = "b"

-- | Prints "k" , "q" , "b" , "n" , "r" or "p"
instance Show Piece where
  show King   = "k"
  show Queen  = "q"
  show Bishop = "b"
  show Knight = "n"
  show Rook   = "r"
  show Pawn   = "p"

-- | Combines piece ++ player. (e.g. "pb" for pawn black)
instance Show PlayerPiece where
  show (PlayerPiece _ player piece) = (show player) ++ (show piece)
  -- show (PlayerPiece uniq_id player piece) = (show player) ++ (show piece) ++ "-" ++ (show uniq_id)
