{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}

module Erik.MyLimitWindows (
    limitWindows,

    increaseLimit,decreaseLimit,setLimit,toggleFull,toggleLimit,

    LimitWindows,

    getCurrentState,updateCurrentState,
    LimitState(..),

    visible,

    rotateVisibleDown,rotateVisibleUp,rotateFocHiddenUp,rotateFocHiddenDown,bury
    ) where

import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.Operations(sendMessage)
import qualified XMonad.StackSet as W
import Control.Monad(guard,when)
import Control.Applicative((<$>))
import qualified XMonad.Util.ExtensibleState as XS
import Erik.MyStuff

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

-- (s, (l1, l2))
-- s        is a normal stack from xmonad
-- l1 ++ l2 are the windows that are hidden from view,
--          l2 are the windows that come after the focused window
--          and l1 are the windows that come before
--
--  1  2  3  4  5  6  7  8  9
-- |---u---|-f-|---d---|--l2--|
-- |---u---|---l1--|-f-|---l2-|
type HiddenStack a = (W.Stack a, ([a], [a]))

-- class for calculating the size of stacks
class StackSize a where
  stackSizeM :: Maybe a -> Int
  stackSizeM = maybe 0 stackSize

  stackSize :: a -> Int

instance StackSize (W.Stack a) where
  stackSize (W.Stack _ u d) = 1 + length u + length d

instance StackSize (HiddenStack a) where
  stackSize (s, (l1, l2)) = stackSize s + length l1 + length l2

-- rotate the currently focused window with all hidden ones
rotateFocHiddenUp :: X ()
rotateFocHiddenUp = modifyHidden (rotateFocHidden rotUp)

rotateFocHiddenDown :: X ()
rotateFocHiddenDown = modifyHidden (rotateFocHidden rotDown)

-- rotate only the actually visible windows
rotateVisibleUp :: X ()
rotateVisibleUp = modifyHidden (rotateVisible rotUp)

rotateVisibleDown :: X ()
rotateVisibleDown = modifyHidden (rotateVisible rotDown)

rotateFocHidden :: ([Window] -> [Window]) -> HiddenStack Window -> HiddenStack Window
rotateFocHidden dir (W.Stack f u d, (uu, h)) = let (newf:newh) = dir (f:h)
                                               in (W.Stack newf u d, (uu, newh))

rotateVisible :: ([Window] -> [Window]) -> HiddenStack Window -> HiddenStack Window
rotateVisible dir (s@(W.Stack _ u _), hid) = (diffN (length u) (dir (W.integrate s)), hid)

-- like windows (W.modify' ...) but for HiddenStack a
modifyHidden :: (HiddenStack Window -> HiddenStack Window) -> X ()
modifyHidden f = do
  LimitState{sfull = fu, soff = o, slimit = l} <- getCurrentState
  windows (W.modify' (\s -> let actualVisible | fu = 1
                                              | o  = 0
                                              | otherwise = l
                            in arcVisible $ f (visible actualVisible s)))

-- same as W.differentiate but can choose which window that is focused
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

-- toggle full (only one window at a time), same as limit=1
toggleFull :: X ()
toggleFull = sendMessage LimitFull

-- toggle this feature on or off
toggleLimit :: X ()
toggleLimit = sendMessage LimitToggle

-- Moves the focused window into hidden
bury :: X ()
bury = do
  LimitState{slimit = l, soff = o, sfull = f} <- getCurrentState
  if f || l == 1
    then return ()
    else do
       size <- stackSizeM . W.stack . W.workspace . W.current <$> gets windowset
       when o $ setLimit size >> toggleLimit
       when (not o && l > size) $ setLimit size
       modifyHidden putLast
       decreaseLimit
  where
    putLast :: HiddenStack a -> HiddenStack a
    putLast (s, hid) | null (W.down s) = (W.focusUp' s, hid)
    putLast (W.Stack f u d, hid) = (W.Stack f' u d', hid)
      where (f':d') = rotUp (f:d)

-- | Only display the first @n@ windows.
-- n, full and off are the default values
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

data LimitState = LimitState { slimit          :: Int,
                               sfull           :: Bool,
                               soff            :: Bool,
                               shidden         :: Int,
                               svisible        :: Int,
                               sdetachedOffset :: Int } deriving (Read,Show)

data SliceStyle = FirstN deriving (Read,Show)

data LimitChange = LimitQuery | LimitToggle | LimitFull | LimitChange { unLC :: Int -> Int } deriving (Typeable)

instance Message LimitChange

instance ExtensionClass LimitState where
  initialValue = LimitState { slimit          = 0,
                              sfull           = False,
                              soff            = False,
                              shidden         = 0,
                              svisible        = 0,
                              sdetachedOffset = 0 }

instance LayoutModifier LimitWindows a where
  handleMess lw@LimitWindows{..} mes
    | Just LimitChange{unLC=f} <- fromMessage mes =
        return $ do
        newLimit <- (f `app` llimit) >>= pos
        return $ lw { llimit = newLimit }
    | Just LimitToggle <- fromMessage mes = return $ Just $ lw { loff  = not loff }
    | Just LimitFull   <- fromMessage mes = return $ Just $ lw { lfull = not lfull }
    | Just LimitQuery  <- fromMessage mes = updateCurrentState >> return Nothing
    | otherwise = return Nothing
    where
      pos x   = guard (x>=1)     >> return x
      app f x = guard (f x /= x) >> return (f x)
      updateCurrentState :: X ()
      updateCurrentState = do
        wor <- W.workspace . W.current <$> gets windowset
        let sta = W.stack wor
            (s, (s1, s2)) = visibleSizes (if lfull then 1 else llimit) sta
            newState = LimitState { slimit          = llimit,
                                    sfull           = lfull,
                                    soff            = loff,
                                    shidden         = s1 + s2,
                                    svisible        = s,
                                    sdetachedOffset = s1}
        XS.put newState
          where visibleSizes _ Nothing = (0, (0, 0))
                visibleSizes n (Just s) = (stackSize h0, (length h1, length h2))
                  where (h0, (h1, h2)) = visible n s

  modifyLayout LimitWindows{..} ws r
    | lfull || not loff = runLayout ws { W.stack = f llimit <$> W.stack ws } r
    | otherwise = runLayout ws r
    where f | lfull     = full
            | otherwise = case lstyle of
                            FirstN -> firstN

getCurrentState :: X LimitState
getCurrentState = XS.get

updateCurrentState :: X ()
updateCurrentState = sendMessage LimitQuery

-- converts a normal stack to a HiddenStack, n is the amount of windows that should be visible
-- on the screen
visible :: Int -> W.Stack a -> HiddenStack a
visible 0 s               = (s, ([], []))
visible n (W.Stack f u d) = (W.Stack f ud du, (ddu, ddd))
  where
    (uu, ud)   = splitAt (length u - (n - 1)) u
    (du, dd)   = splitAt (n - (length ud + 1)) (reverse uu ++ d)
    (ddu, ddd) = splitAt (length uu) dd

arcVisible :: HiddenStack a -> W.Stack a
arcVisible (W.Stack f u d, (h1, h2)) = W.Stack f (reverse h1 ++ u) (d ++ h2)

firstN :: Int -> W.Stack a -> W.Stack a
firstN n st = fst $ visible n st

full :: Int -> W.Stack a -> W.Stack a
full _ = firstN 1

