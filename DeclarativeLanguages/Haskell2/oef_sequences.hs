import Data.Char (ord, chr)


class Sequence a where
  next :: a -> a
  prev :: a -> a

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a


instance Sequence Int where
  next x = x + 1
  prev x = x - 1
-- OR
  -- next = pred
  -- prev = succ

instance LeftBoundedSequence Int where
  firstElem = minBound

instance RightBoundedSequence Int where
  lastElem = maxBound


instance Sequence Bool where
  next = not
  prev = not

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Bool where
  lastElem = True

instance Sequence Char where
  next 'z' = error "no value after 'z'"
  next c   = chr $ next $ ord c -- OR: succ c
  prev 'a' = error "no value before 'a'"
  prev c   = chr $ prev $ ord c -- OR: pred c

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance RightBoundedSequence Char where
  lastElem = 'z'
