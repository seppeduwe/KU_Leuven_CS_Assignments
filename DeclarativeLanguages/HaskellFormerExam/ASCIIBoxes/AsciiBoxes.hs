
module AsciiBoxes where

import Data.List (intersperse)

type Width  = Int
type Height = Int
type Point  = (Int, Int)

data Box = Box (Width, Height) (Point -> Char)

-- Render a Box as a String
-- ~~~~~~~~~~~~~~~~~~~~~~~~
renderBox :: Box -> String
-- renderBox (Box (w,h) f) = undefined
renderBox (Box (w,h) f) = unlines $ reverse [ [ f (x,y) | x <- [0..(w-1)] ] | y <- [0..(h-1)] ]

instance Show Box where
  show = undefined

-- Basic Boxes
-- ~~~~~~~~~~~
emptyBox :: Box
emptyBox = undefined

constantBox :: (Width, Height) -> Char -> Box
constantBox dim c = undefined

-- Reasoning About Points
-- ~~~~~~~~~~~~~~~~~~~~~~
inArea :: Point -> Point -> (Width, Height) -> Bool
inArea (x,y) (xp,yp) (w,h) = undefined

-- Box Combinators
-- ~~~~~~~~~~~~~~~
beside :: Box -> Box -> Box
beside (Box (w1,h1) r1) (Box (w2,h2) r2) = undefined

above :: Box -> Box -> Box
above (Box (w1,h1) r1) (Box (w2,h2) r2) = undefined

wrapBox :: Char -> Box -> Box
wrapBox c (Box (w,h) r) = undefined

overlay :: Box -> Box -> Box
overlay (Box (w1,h1) r1) (Box (w2,h2) r2) = undefined

besideMany :: [Box] -> Box
besideMany bs = undefined

-- Boxable Class & Instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
class Boxable a where
  toBox :: a -> Box

instance Boxable Char where
  toBox c = Box (1,1) (\_ -> c)
            -- undefined

instance Boxable a => Boxable [a] where
  toBox xs = undefined

-- Making Histograms
-- ~~~~~~~~~~~~~~~~~
makeHistogram :: [(Char, Int)] -> Box
makeHistogram = undefined
