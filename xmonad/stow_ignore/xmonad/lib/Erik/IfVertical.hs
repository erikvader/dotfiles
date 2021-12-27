{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Erik.IfVertical (ifVertical) where

import XMonad
import XMonad.StackSet

data IfVertical l1 l2 a = IfVertical Bool (l1 a) (l2 a) deriving (Read, Show)

verticalFromMaybeV (IfVertical _ ver hor) Nothing = Just $ IfVertical True ver hor
verticalFromMaybeV (IfVertical _ _ hor) (Just l)  = Just $ IfVertical True l hor

verticalFromMaybeH (IfVertical _ ver hor) Nothing = Just $ IfVertical False ver hor
verticalFromMaybeH (IfVertical _ ver _) (Just l)  = Just $ IfVertical False ver l

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (IfVertical l1 l2) a where
  runLayout (Workspace tag old@(IfVertical lastVert vertLay horLay) a) rect@(Rectangle _ _ w h)
    | w > h     = (verticalFromMaybeH old <$>) <$> runLayout (Workspace tag horLay a) rect
    | otherwise = (verticalFromMaybeV old <$>) <$> runLayout (Workspace tag vertLay a) rect

  handleMessage (IfVertical lastVert vertLay horLay) msg
    | lastVert  = (flip (IfVertical lastVert) horLay <$>) <$> handleMessage vertLay msg
    | otherwise = (IfVertical lastVert vertLay <$>)       <$> handleMessage horLay msg

  description (IfVertical lastVert vertLay horLay)
    | lastVert  = description vertLay
    | otherwise = description horLay

-- A layout modifier that alternates between two other layouts
-- depending on whether the display that this layout is on is wider
-- than its height. This is an alternate take on
-- XMonad.Layout.PerScreen.
--
-- If the current screen is vertical (height is larger than width)
-- then the first layout is used. If the screen is wide then the
-- second layout is used.
ifVertical :: (LayoutClass l1 a, LayoutClass l2 a) => l1 a -> l2 a -> IfVertical l1 l2 a
ifVertical = IfVertical False
