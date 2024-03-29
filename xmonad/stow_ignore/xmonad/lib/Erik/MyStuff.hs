module Erik.MyStuff (
  rotLastUp, rotLastDown, rotLast',
  rotUp, rotDown,
  onLayout,
  windowsLowestEmpty,
  shiftView,
  mapWorkspaces,
  swapWith,
  centerFloat,
  myUpdatePointer, myUpdatePointerToggle,
  notifySend,
  ppShowWindows,
  decoratePP,
  toggleMapStruts,
  switchScreen,
  banish
) where

import Prelude hiding (log)
import XMonad hiding (mapped)
import qualified XMonad.StackSet as W
import Data.List (find, elemIndex)
import Data.Maybe (isNothing,isJust,fromMaybe,mapMaybe,listToMaybe)
import Control.Monad (unless,filterM)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Util.ExtensibleState as XS
import Data.Bits (testBit)
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import Control.Exception (catch,SomeException)
import Data.Char (toLower)
import XMonad.Hooks.ManageDocks (checkDock)
import Text.Regex
import XMonad.Actions.PhysicalScreens
import Data.Ratio
import XMonad.Actions.Warp (warpToWindow)
import Data.Bifunctor (first)

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
onLayout xs deff = do
  d <- description . W.layout . W.workspace . W.current <$> gets windowset
  case find ((== d) . fst) xs of
    Just (_, x) -> x
    Nothing -> deff

-- runs f on the lowest index empty hidden or current workspace, if there is one
windowsLowestEmpty :: (WorkspaceId -> WindowSet -> WindowSet) -> [String] -> X ()
windowsLowestEmpty f order = windows (\w -> maybe id f (findLowestEmpty w) w)
  where
    findLowestEmpty :: WindowSet -> Maybe WorkspaceId
    findLowestEmpty w = find g order
      where
        g i = maybe False (isNothing . W.stack) $ find ((i ==) . W.tag) candidates
        candidates = (:[]) . W.workspace . W.current <> W.hidden $ w

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

----------------------------- my update pointer -----------------------------

newtype MyUpdatePointerActive = MyUpdatePointerActive Bool
instance ExtensionClass MyUpdatePointerActive where
  initialValue = MyUpdatePointerActive True

myUpdatePointerToggle :: X ()
myUpdatePointerToggle = XS.get >>= (\m -> log m >> XS.put m) . toggle
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

------------------------------ pp show windows ------------------------------

-- decorates PP to run f first on all workspace related rules
decoratePP :: (WorkspaceId -> String) -> PP -> PP
decoratePP f pp = pp {
  ppCurrent         = ppCurrent pp . f,
  ppVisible         = ppVisible pp . f,
  ppHidden          = ppHidden pp . f,
  ppHiddenNoWindows = ppHiddenNoWindows pp . f,
  ppUrgent          = ppUrgent pp . f
  }

-- get all WM_CLASS property values from a window
getClass :: Window -> X [String]
getClass w = withDisplay $ \dsp -> io $
  catch
    (getTextProperty dsp w wM_CLASS >>= wcTextPropertyToTextList dsp)
    ((\_ -> return []) :: SomeException -> IO [String])

defaultIcon = "#"
-- default settings on mkRegex might be wrong (not sure)
windowIcons = map (first mkRegex) [
  ("^mpv$", "m"),
  ("^MATLAB", "m")
  ]

regexLookup :: String -> [(Regex, String)] -> Maybe String
regexLookup s tab = snd <$> find (isJust . flip matchRegex s . fst) tab

-- returns a function that retreives a workspace's window string
-- (string containing one char for each window)
ppShowWindows :: X (WorkspaceId -> String)
ppShowWindows = getIcons
  where
    getWindowsFor :: WindowSet -> WorkspaceId -> [Window]
    getWindowsFor set wi = maybe [] (W.integrate' . W.stack) (find (\w -> wi == W.tag w) $ W.workspaces set)

    classesToIcon :: [String] -> String
    classesToIcon cs = fromMaybe defaultIcon $ listToMaybe $ mapMaybe (`regexLookup` windowIcons) cs ++ mapMaybe (fmap ((:[]) . toLower) . listToMaybe) cs

    getIcons :: X (WorkspaceId -> String)
    getIcons = do
      ss <- gets windowset
      let allwins = W.allWindows ss
      withClasses <- M.fromList . zip allwins <$> mapM getClass allwins
      return $ \ws -> concatMap (\w -> classesToIcon $ M.findWithDefault [] w withClasses) $ getWindowsFor ss ws

-------------------------------- unmap struts -------------------------------

-- toggle visibility (mapping) of all docks on the current screen
toggleMapStruts :: X ()
toggleMapStruts = do
  docks <- gets windowset >>= getDocksOn . W.screen . W.current
  allMapped <- and <$> mapM isMapped docks
  setMappingOf (not allMapped) docks

-- if showit is true, then map all windows in ws
--                    else unmap all windows in ws
setMappingOf :: Bool -> [Window] -> X ()
setMappingOf showit = mapM_ (\w -> withDisplay $ \d -> io $ f d w)
  where f | showit = mapWindow
          | otherwise = unmapWindow

-- get all dock windows according to checkDock from XMonad.Hooks.ManageDocks
getDocks :: X [Window]
getDocks = do
  d <- asks display
  r <- asks theRoot
  (_,_,ws) <- io $ queryTree d r
  filterM (runQuery checkDock) ws

-- get all dock windows on a particular screen
getDocksOn :: ScreenId -> X [Window]
getDocksOn si = filterM (`isWindowOnScreen` si) =<< getDocks

-- checks whether w is on screen with id si
isWindowOnScreen :: Window -> ScreenId -> X Bool
isWindowOnScreen w si = (si ==) . fst <$> floatLocation w

-- checks if w is mapped (visible) or not
-- waIsUnviewable counts as unmapped in this case (this is set if a
-- child window is mapped to an unmapped parent)
isMapped :: Window -> X Bool
isMapped w = withDisplay $ \d -> do
  attr <- io $ getWindowAttributes d w
  return $ wa_map_state attr == waIsViewable

-------------------------------- visit screens --------------------------------

-- takes a function f that determines the next screen to change focus to.
-- f :: numberOfScreens -> currentScreen -> nextScreenToFocus
-- All screens are physical screen numbers
switchScreen :: ScreenComparator -> (Int -> Int -> Int) -> X ()
switchScreen sc nextScreen = do
  numScreens <- length . W.screens <$> gets windowset
  mapped <- mapM (getScreen sc . P) [0..(numScreens - 1)]
  curScreen <- gets (W.screen . W.current . windowset)
  whenJust (elemIndex (Just curScreen) mapped) $ \focusedPhysScreen -> do
    let next = nextScreen numScreens focusedPhysScreen
    viewScreen sc (P next)


-------------------------------- banish pointer -------------------------------

-- withFocused with a default value in case no window is focused
withFocusedDef :: a -> (Window -> X a) -> X a
withFocusedDef deff f = withWindowSet $ \w -> maybe (return deff) f (W.peek w)

cursorPosition :: X (Maybe (Rational, Rational))
cursorPosition =
  withDisplay $ \d ->
    withFocusedDef Nothing $ \w -> do
      wa <- io $ getWindowAttributes d w
      (insideWindow, _, _, _, _, x, y, _) <- io $ queryPointer d w
      return $ if insideWindow
               then Just (fromIntegral x % fromIntegral (wa_width wa),
                          fromIntegral y % fromIntegral (wa_height wa))
               else Nothing

banish :: X ()
banish = cursorPosition >>= flip whenJust (uncurry f)
  where f 0 0 = warpToWindow 0 1
        f 1 1 = warpToWindow 1 0
        f 1 0 = warpToWindow 0 0
        f _ _ = warpToWindow 1 1
