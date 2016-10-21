data Suit = Club | Diamond | Heart | Spade deriving (Show, Eq)

data Teletubby = Po | Dipsy | LaaLaa | TinkyWinky deriving (Show, Eq)

class MaybeRed a where
  isRed :: a -> Bool

instance MaybeRed Teletubby where
  isRed Po = True
  isRed _ = False

instance MaybeRed Suit where 
  isRed Diamond = True
  isRed Heart = True
  isRed _ = False

getReds :: (MaybeRed a) => [a] -> [a]
getReds xs = filter isRed xs


