{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Erik.DoubleMaster (DoubleMaster(..), MasterResize(..)) where

import XMonad
import XMonad.StackSet hiding (stack)
import Data.List

inc = 3/100

-- DoubleMaster masterHeights middleSplit
data DoubleMaster a = DoubleMaster Rational Rational deriving (Show,Read)
data MasterResize = MasterShrink | MasterExpand deriving (Typeable)

instance Message MasterResize

instance LayoutClass DoubleMaster a where
  -- pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout (DoubleMaster r s) rect stack
    | [w] <- integrate stack = [(w, rect)]
    | otherwise              = line halve1 w1 ++ reflect (line halve2 w2)
    where
      -- mapBoth f (a,b) = (f a, f b)
      -- (w1, w2) = mapBoth (map snd) . partition (even . fst) . zip [0..] $ integrate stack
      ([h1, h2], rest) = splitAt 2 $ integrate stack
      (rest1, rest2) = (\x -> splitAt (length x `div` 2) x) rest
      (w1, w2) = (h1:rest1, h2:rest2)

      (halve1, halve2) = splitHorizontallyBy s rect

      line :: Rectangle -> [a] -> [(a, Rectangle)]
      line _ [] = []
      line rectt [w] = [(w, rectt)]
      line rectt (w:ws) =
        let
          (r1, rs) = splitVertically (length ws) <$> splitVerticallyBy r rectt
        in (w, r1) : zip ws rs

      reflect ws = let (wins, rects) = unzip $ reverse ws
                       start = rect_y $ last rects
                       newRects = zipWith (\(Rectangle x _ w h) y -> Rectangle x y w h) rects . map ((+start) . fromIntegral) . (0:) . init . map sum . tail . inits . map rect_height $ rects
                   in zip wins newRects

  -- emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
  emptyLayout _ _ = return ([], Nothing)

  -- pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
  pureMessage (DoubleMaster r s) m | Just MasterShrink <- fromMessage m = Just (DoubleMaster (max 0 $ r-inc) s)
  pureMessage (DoubleMaster r s) m | Just MasterExpand <- fromMessage m = Just (DoubleMaster (min 1 $ r+inc) s)
  pureMessage (DoubleMaster r s) m | Just Shrink <- fromMessage m = Just (DoubleMaster r (max 0 $ s-inc))
  pureMessage (DoubleMaster r s) m | Just Expand <- fromMessage m = Just (DoubleMaster r (min 1 $ s+inc))
  pureMessage _ _ = Nothing

  description _ = "DoubleMaster"
