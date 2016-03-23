{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Snap.Core
import Snap.Snaplet
import Control.Lens
import qualified Data.ByteString.Char8 as B
import Api.Services.BoardService (BoardService(), boardServiceInit)
import Api.Services.MoveService (MoveService(), moveServiceInit)

data Api = Api { _boardService :: Snaplet BoardService
               , _moveService :: Snaplet MoveService }
makeLenses ''Api

type ApiHandler b = Handler b Api ()

apiRoutes :: [(B.ByteString, ApiHandler b)]
apiRoutes = [("status", method GET respondOk)]

respondOk :: ApiHandler b
respondOk = modifyResponse $ setResponseCode 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  bs <- nestSnaplet "board" boardService boardServiceInit
  ms <- nestSnaplet "moves" moveService $ moveServiceInit bs
  addRoutes apiRoutes
  return $ Api bs ms
