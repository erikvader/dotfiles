{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LimitWindows
-- Copyright   :  (c) 2009 Adam Vogt
--                (c) 2009 Max Rabkin -- wrote limitSelect
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that limits the number of windows that can be shown.
-- See "XMonad.Layout.Minimize" for manually setting hidden windows.
--
-----------------------------------------------------------------------------

module Erik.MyLimitWindows (
    -- * Usage
    -- $usage

    -- * Layout Modifiers
    limitWindows,

    -- * Change the number of windows
    increaseLimit,decreaseLimit,setLimit,toggleFull,toggleLimit,

    -- * Types
    LimitWindows,

    initStates,getCurrentState,

    visible,stackSize,

    rotateVisibleDown,rotateVisibleUp,rotateFocHiddenUp,rotateFocHiddenDown
    ) where

import XMonad.Layout.LayoutModifier
import XMonad
import qualified XMonad.StackSet as W
import Control.Monad((<=<),guard,when)
import Control.Applicative((<$>))
import Data.Maybe(fromJust,isJust)
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map.Lazy as Map
import Erik.MyStuff (rotUp,rotDown)

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LimitWindows
--
-- > myLayout = limitWindows 6 $ Tall 1 0.03 0.5 ||| Full ||| RandomOtherLayout...
-- > main = xmonad def { layoutHook = myLayout }
--
-- You may also be interested in dynamically changing the number dynamically,
-- by binding keys to the 'increaseLimit', 'decreaseLimit', or 'setLimit'
-- actions.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- See also 'XMonad.Layout.BoringWindows.boringAuto' for keybindings that skip
-- the hidden windows.

rotateFocHiddenUp :: X ()
rotateFocHiddenUp = rotateFocHidden rotUp

rotateFocHiddenDown :: X ()
rotateFocHiddenDown = rotateFocHidden rotDown

rotateFocHidden :: ([Window] -> [Window]) -> X ()
rotateFocHidden rot = do
  state <- getCurrentState
  windows (W.modify' (f state))
  where
    f :: Maybe (Int, Bool, Bool) -> W.Stack Window -> W.Stack Window
    f Nothing s = s
    f (Just (limit, full, off)) s
      | full      = f (Just (1, False, off)) s
      | off       = s
      | otherwise = let (W.Stack f u d, (uu, h)) = visible limit s
                        (newf:newh) = rot (f:h)
                    in arcVisible (W.Stack newf u d, (uu, newh))

rotateVisibleUp :: X ()
rotateVisibleUp = rotateVisible rotUp

rotateVisibleDown :: X ()
rotateVisibleDown = rotateVisible rotDown

rotateVisible :: ([Window] -> [Window]) -> X ()
rotateVisible rot = do
  state <- getCurrentState
  windows (W.modify' (f state))
  where
    f :: Maybe (Int, Bool, Bool) -> W.Stack Window -> W.Stack Window
    f Nothing s = s
    f (Just (limit, full, off)) s
      | full       = f (Just (1,                  False, off))   s
      | off        = f (Just (stackSize (Just s), full,  False)) s
      | limit == 1 = s
      | otherwise  = let (sta@(W.Stack _ u _), tup) = visible limit s
                     in arcVisible (diffN (length u) (rot (W.integrate sta)), tup)

diffN :: Int -> [a] -> W.Stack a
diffN _ [] = error "går int, borde int vara tom"
diffN n as = case splitAt n as of
               (u, []) -> let (f:uu) = reverse u
                          in W.Stack f uu []
               (u, f:d) -> W.Stack f (reverse u) d

increaseLimit :: X ()
increaseLimit = sendMessage $ LimitChange succ

decreaseLimit :: X ()
decreaseLimit = sendMessage . LimitChange $ max 1 . pred

setLimit :: Int -> X ()
setLimit tgt = sendMessage . LimitChange $ const tgt

toggleFull :: X ()
toggleFull = sendMessage LimitFull

toggleLimit :: X ()
toggleLimit = sendMessage LimitToggle

-- | Only display the first @n@ windows.
limitWindows :: Int -> Bool -> Bool -> l a -> ModifiedLayout LimitWindows l a
limitWindows n full off = ModifiedLayout (LimitWindows FirstN n full off)

-- | Only display @n@ windows around the focused window. This makes sense with
-- layouts that arrange windows linearily, like 'XMonad.Layout.Layout.Accordion'.
-- limitSlice :: Int -> l a -> ModifiedLayout LimitWindows l a
-- limitSlice n = ModifiedLayout (LimitWindows Slice n False False)

-- | Only display the first @m@ windows and @r@ others.
-- The @IncMasterN@ message will change @m@, as well as passing it onto the
-- underlying layout.
-- limitSelect :: Int -> Int -> l a -> ModifiedLayout Selection l a
-- limitSelect m r = ModifiedLayout Sel{ nMaster=m, start=m, nRest=r }

data LimitWindows a = LimitWindows { lstyle :: SliceStyle,
                                     llimit :: Int,
                                     lfull  :: Bool,
                                     loff   :: Bool} deriving (Read,Show)

-- limit, full, off
newtype LimitState = LimitState (Map.Map WorkspaceId (Int, Bool, Bool)) deriving (Read,Show)

data SliceStyle = FirstN | Slice deriving (Read,Show)

data LimitChange = LimitToggle | LimitFull | LimitChange { unLC :: Int -> Int } deriving (Typeable)

instance Message LimitChange

instance ExtensionClass LimitState where
  initialValue = LimitState Map.empty

instance LayoutModifier LimitWindows a where
  handleMess lw mes = do
    x <- handleMess' lw mes
    when (isJust x) (updateCurrentState $ fromJust x)
    return x
    where
      handleMess' lw@LimitWindows{..} mes
        | Just LimitChange{unLC=f} <- fromMessage mes =
            return $ do
            newLimit <- (f `app` llimit) >>= pos
            return $ lw { llimit = newLimit }
        | Just LimitToggle <- fromMessage mes = return $ Just $ lw { loff  = not loff }
        | Just LimitFull   <- fromMessage mes = return $ Just $ lw { lfull = not lfull }
        | otherwise = return Nothing
      pos x   = guard (x>=1)     >> return x
      app f x = guard (f x /= x) >> return (f x)

  modifyLayout LimitWindows{..} ws r
    | lfull || not loff = runLayout ws { W.stack = f llimit <$> W.stack ws } r
    | otherwise = runLayout ws r
    where f | lfull     = full
            | otherwise = case lstyle of
                            FirstN -> firstN

getCurrentState :: X (Maybe (Int, Bool, Bool))
getCurrentState = do
  id <- W.tag . W.workspace . W.current <$> gets windowset
  (LimitState states) <- XS.get
  return $ Map.lookup id states

updateCurrentState :: LimitWindows a -> X ()
updateCurrentState lw = do
  id <- W.tag . W.workspace . W.current <$> gets windowset
  (LimitState old) <- XS.get
  XS.put $ LimitState $ Map.insert id (llimit lw, lfull lw, loff lw) old

initStates :: [WorkspaceId] -> Int -> Bool -> Bool -> X ()
initStates ws l f o = XS.put $ LimitState $ Map.fromList [(k, (l,f,o)) | k <- ws]

stackSize :: Maybe (W.Stack a) -> Int
stackSize Nothing                = 0
stackSize (Just (W.Stack _ u d)) = 1 + length u + length d

visible :: Int -> W.Stack a -> (W.Stack a, ([a], [a]))
visible n (W.Stack f u d) = (W.Stack f ud du, (ddu, ddd))
  where
    (uu, ud)   = splitAt (length u - (n - 1)) u
    (du, dd)   = splitAt (n - (length ud + 1)) (reverse uu ++ d)
    (ddu, ddd) = splitAt (length uu) dd

arcVisible :: (W.Stack a, ([a], [a])) -> W.Stack a
arcVisible (W.Stack f u d, (h1, h2)) = W.Stack f (reverse h1 ++ u) (d ++ h2)

firstN :: Int -> W.Stack a -> W.Stack a
firstN n st = fst $ visible n st

full :: Int -> W.Stack a -> W.Stack a
full _ = firstN 1

