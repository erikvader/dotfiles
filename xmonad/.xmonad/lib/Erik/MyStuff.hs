{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
module Erik.MyStuff (
  rotLastUp, rotLastDown, rotLast',
  rotUp, rotDown,
  onLayout,
  windowsLowestEmpty,
  shiftView,
  mapWorkspaces,
  swapWith,
  workspaceNamesClearerLogHook,
  centerFloat,
  myUpdatePointer, myUpdatePointerToggle,
  notifySend
  -- pointerDance
) where

import XMonad
import qualified XMonad.StackSet as W
import Data.List (find)
import Data.Maybe (maybe,isNothing,isJust)
import Control.Monad (when, unless)
import XMonad.Actions.WorkspaceNames (getWorkspaceNames',setWorkspaceName)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Util.ExtensibleState as XS
import Data.Bits (testBit)
import XMonad.Util.Run (safeSpawn)

-- pointerDance (num of jumps) (delay in microseconds)
-- pointerDance :: Int -> Int -> X ()
-- pointerDance n t = do
--   pos <- warps
--   sequence_ $ tail $ (++[head sleeps]) . concat $ zipWith (\a b -> [a,b]) sleeps pos
--   where
--     warps :: X [X ()]
--     warps = do
--       ls <- io $ rposes n [] 0 >>= return . (++ [(0.5, 0.5)])
--       return $ map (uncurry warpToWindow) (map (\(a, b) -> (toRational a, toRational b)) ls)

--     sleeps = repeat (io $ threadDelay t)

--     rposes :: Int -> [(Double, Double)] -> Int -> IO [(Double, Double)]
--     rposes 0 l _ = return l
--     rposes n l a = do
--       x <- randomRIO (0.1, 0.9)
--       y <- randomRIO (0.1, 0.9)
--       let t = (x, y) in case l of
--                           [] -> rposes (n-1) [t] a
--                           (x:xs) | a >= 3 || good t x -> rposes (n-1) (t:x:xs) 0
--                                  | otherwise          -> rposes n (x:xs) (a + 1)

--     good (x1, y1) (x2, y2) = ((x1-x2) ** 2) + ((y1-y2) ** 2) < (100 ** 2)

notifySend :: String -> String -> X ()
notifySend header msg = safeSpawn "notify-send" [header, msg]

getCurrentScreenSize :: X Rectangle
getCurrentScreenSize = screenRect . W.screenDetail . W.current . windowset <$> get

centerFloat :: Dimension -> Dimension -> Window -> X ()
centerFloat w h win = do
  Rectangle sx sy sw sh <- getCurrentScreenSize
  tileWindow win (Rectangle
                  (sx + fromIntegral ((sw - w) `div` 2))
                  (sy + fromIntegral ((sh - h) `div` 2))
                  w
                  h)
  float win

shiftView :: WorkspaceId -> WindowSet -> WindowSet
shiftView i = W.view i . W.shift i

rotLastUp :: X ()
rotLastUp = windows $ W.modify' (rotLast' rotUp)

rotLastDown :: X ()
rotLastDown = windows $ W.modify' (rotLast' rotDown)

-- rotate the lower windows in the stack (including focused)
-- with a given function (rotate direction)
rotLast' :: ([a] -> [a]) -> W.Stack a -> W.Stack a
rotLast' f (W.Stack t l r) = W.Stack t' l r'
  where
    (t':r') = f (t:r)

-- $generic
-- Generic list rotations such that @rotUp [1..4]@ is equivalent to
-- @[2,3,4,1]@ and @rotDown [1..4]@ to @[4,1,2,3]@. They both are
-- @id@ for null or singleton lists.
rotUp :: [a] -> [a]
rotUp   l = drop 1 l ++ take 1 l
rotDown :: [a] -> [a]
rotDown = reverse . rotUp . reverse

-- runs a X command(?) depending on the description of the current
-- layout. Useful for assigning layout specific keybinds.
onLayout :: [(String, X a)] -> X a -> X a
onLayout xs def = do
  d <- description . W.layout . W.workspace . W.current <$> gets windowset
  case find ((== d) . fst) xs of
    Just (_, x) -> x
    Nothing -> def

-- runs f on the lowest index empty hidden workspace, if there is one
windowsLowestEmpty :: (WorkspaceId -> WindowSet -> WindowSet) -> [String] -> X ()
windowsLowestEmpty f order = windows (\w -> maybe id f (findLowestEmpty w) w)
  where
    findLowestEmpty :: WindowSet -> Maybe WorkspaceId
    findLowestEmpty w = find f order
      where
        f id = maybe False (isNothing . W.stack) $ find ((id ==) . W.tag) (W.hidden w)

mapWorkspaces :: (WorkspaceId -> X a) -> X()
mapWorkspaces f = asks (workspaces . config) >>= mapM_ f

-- swap current workspace with wi and focus current
-- only makes sense if both current and wi are visible on separate monitors
swapWith :: WorkspaceId -> WindowSet -> WindowSet
swapWith wi ws = W.view cur . W.greedyView wi $ ws
  where cur = W.tag . W.workspace . W.current $ ws

-- Xmonad.Actions.WorkspaceNames auto clearer -------------------

newtype WorkspaceNamesCache = WorkspaceNamesCache String
instance ExtensionClass WorkspaceNamesCache where
  initialValue = WorkspaceNamesCache ""

-- clears the name of a hidden workspace if that workspace is empty and if it has a name
workspaceNamesClearerLogHook :: X ()
workspaceNamesClearerLogHook = do
  ss <- gets windowset
  let hash = concatMap hashfun $ W.current ss:W.visible ss
  (WorkspaceNamesCache oldHash) <- XS.get
  when (hash /= oldHash) $ do
    clearAllEmpty (W.hidden ss)
    XS.put $ WorkspaceNamesCache hash
  where
    hashfun s = concat [show $ W.screen s, show $ W.tag . W.workspace $ s, show $ W.screenDetail s]

    clearAllEmpty :: [WindowSpace] -> X ()
    clearAllEmpty hiddens = do
      gwn <- getWorkspaceNames'
      -- predikatet måste gå att göra finare
      let cands = filter (\x -> (isNothing . W.stack) x && (isJust . gwn . W.tag) x) hiddens
      mapM_ (flip setWorkspaceName "" . W.tag) cands

----------------------------- my update pointer -----------------------------

newtype MyUpdatePointerActive = MyUpdatePointerActive Bool
instance ExtensionClass MyUpdatePointerActive where
  initialValue = MyUpdatePointerActive True

myUpdatePointerToggle :: X ()
myUpdatePointerToggle = toggle <$> XS.get >>= \m -> log m >> XS.put m
  where
    toggle (MyUpdatePointerActive b) = MyUpdatePointerActive (not b)
    log (MyUpdatePointerActive b) = notifySend ("pointer update is "++if b then "on" else "off") ""

-- Do the same thing as XMonad.Actions.UpdatePointer, except that it
-- also checks whether a mouse button is currently pressed. If one is
-- pressed, then that probably means that something is being dragged,
-- and if something is being dragged we don't want the cursor to jump
-- all over the place. So if a mouse button is pressed, this does
-- nothing.
myUpdatePointer =
  whenX isActive $ do
    dpy <- asks display
    root <- asks theRoot
    (_,_,_,_,_,_,_,m) <- io $ queryPointer dpy root
    unless (testBit m 9 || testBit m 8 || testBit m 10) $
      updatePointer (0.5, 0.5) (0.25, 0.25)

  where
    isActive = (\(MyUpdatePointerActive b) -> b) <$> XS.get

