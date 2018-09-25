{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Destroyer where

import Common.Redux
import Common.Components.Entity

data Destroy = Destroy EntityId deriving ReduxEvent

destroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
destroy (Destroy entityId) entities = return $ filter (\e -> from e /= Just entityId) entities

destroyer :: Redux [ Entity ]
destroyer = noOpRedux { reducer = focusM destroy }
