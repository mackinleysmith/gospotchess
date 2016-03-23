{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api.Services.BoardService where

import Api.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Control.Lens
import Snap.Core
import Snap.Snaplet
import ChessBoard
import Data.Aeson
import Control.Monad.State
import Data.Typeable (Typeable)
import Control.Concurrent

data GameState = GameState { _moveCounter :: Int, _board :: Board } deriving (Show,Eq,Typeable)
makeLenses ''GameState

initializeGameState :: GameState
initializeGameState = GameState { _board = initBoard, _moveCounter = 0 }

data BoardService = BoardService { _gameStateVar :: MVar GameState }
makeLenses ''BoardService

boardRoutes :: [(B.ByteString, Handler b BoardService ())]
boardRoutes = [ ("modify", method GET modifyBoard)
              , ("list",   method GET listBoard)
              , ("show",   method GET showBoard) ]

readBoard :: MVar GameState -> IO Board
readBoard gs_var = do
  current_gs <- readMVar gs_var
  return $ _board current_gs

updateBoard :: MVar GameState -> (Board -> Board) -> IO ()
updateBoard gs_var board_fn = do
  current_gs <- takeMVar gs_var
  putMVar gs_var $! over board board_fn current_gs
  return ()

setBoard :: MVar GameState -> Board -> IO ()
setBoard gs_var new_board = do
  current_gs <- takeMVar gs_var
  putMVar gs_var $! set board new_board current_gs
  return ()

listBoard :: Handler b BoardService ()
listBoard = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  current_board <- liftIO . readBoard =<< view gameStateVar
  writeLBS . encode . JsonBoard $ current_board

showBoard :: Handler b BoardService ()
showBoard = do
  modifyResponse $ setHeader "Content-Type" "text/plain"
  current_board <- liftIO . readBoard =<< view gameStateVar
  writeLBS . BL.pack $ show current_board

modifyBoard :: Handler b BoardService ()
modifyBoard = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  maybeFromParam <- getParam "from"
  maybeToParam <- getParam "to"
  case (maybeFromParam, maybeToParam) of
    (Just fromParam, Just toParam) -> do
      case (decodePosition fromParam, decodePosition toParam) of
        (Just fromPos, Just toPos) -> do
          gs_var <- view gameStateVar
          output <- liftIO $ do
            current_board <- readBoard gs_var
            case move current_board fromPos toPos of
              Right (new_board, _) -> do
                setBoard gs_var new_board
                return $ BL.pack "{\"status\": \"ok\"}"
              Left msg -> return $ BL.pack $ "{\"status\": \"not ok\", \"error\": \"" ++ msg ++ "\"}"
          writeLBS output
        _ -> writeLBS "{\"status\": \"not ok\", \"error\": \"Failed to parse one of the positions supplied.\"}"
    _ -> writeLBS "{\"status\": \"not ok\", \"error\": \"You must include both a from and to parameter.\"}"

boardServiceInit :: SnapletInit b BoardService
boardServiceInit = makeSnaplet "board" "Board Service" Nothing $ do
  addRoutes boardRoutes
  let initial_gs = initializeGameState
  gs_var <- liftIO $ do
    gs_var' <- newEmptyMVar
    putMVar gs_var' initial_gs
    return gs_var'
  return $ BoardService gs_var
