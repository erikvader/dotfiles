{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}

module Erik.MyLimitWindows (
    limitWindows,

    increaseLimit,decreaseLimit,setLimit,toggleFull,toggleLimit,

    LimitWindows,

    initStates,getCurrentState,

    visible,stackSize,

    rotateVisibleDown,rotateVisibleUp,rotateFocHiddenUp,rotateFocHiddenDown,bury
    ) where

import XMonad.Layout.LayoutModifier
import XMonad
import qualified XMonad.StackSet as W
import Control.Monad((<=<),guard,when)
import Control.Applicative((<$>))
import Data.Maybe(fromJust,isJust,isNothing)
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
  stackSize :: Maybe a -> Int

instance StackSize (W.Stack a) where
  stackSize Nothing                = 0
  stackSize (Just (W.Stack _ u d)) = 1 + length u + length d

instance StackSize (HiddenStack a) where
  stackSize Nothing              = 0
  stackSize (Just (s, (l1, l2))) = stackSize (Just s) + length l1 + length l2

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
  state <- getCurrentState
  mapM_ (\(l, fu, o) -> windows (W.modify' (\s -> let actualVisible | fu = 1
                                                                    | o  = 0
                                                                    | otherwise = l
                                                  in arcVisible $ f (visible actualVisible s)))) state

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
  state <- getCurrentState
  case state of
    Nothing -> return ()
    (Just (l, f, o)) | f || l == 1 -> return ()
                     | otherwise -> do
                         size <- stackSize . W.stack . W.workspace . W.current <$> gets windowset
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

-- returns the state for the current workspace if there is one
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

-- initialize the states as a extended state so we can access them easily
initStates :: [WorkspaceId] -> Int -> Bool -> Bool -> X ()
initStates ws l f o = XS.put $ LimitState $ Map.fromList [(k, (l,f,o)) | k <- ws]

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

