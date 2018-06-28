module Erik.CompactWorkspaces (
    compWS, compactWindows, generateWorkspaces,
    CompInd
    ) where

import XMonad
import XMonad.StackSet
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer
import Graphics.X11.Xlib.Extras (Event(ClientMessageEvent))
import Data.Monoid (All(..))

-- some kind of index indicating which button was pressed
type CompInd = Int

-- the state
-- First i
--   indicating that we haven't pressed any button yet. i is how many
--   "dimensions" we have
--
-- Second i ci ti f
--   we have pressed one button and are waiting for another to be pressed or for a timer to trigger.
--   i is the "dimension", ci is the previously pressed button, ti is
--   the id for the timer and f is the function to apply on the
--   workspace id
data CompState = First Int | Second Int CompInd TimerId (WorkspaceId -> X ())

instance ExtensionClass CompState where
  initialValue = First 0

-- set the workspaces and add necessary hooks for a config
-- i is the "dimension"
-- there should be atleast i^2 workspaces with any name
compactWindows :: Int -> XConfig l -> XConfig l
compactWindows i c = c {
  startupHook = initState i <+> startupHook c,
  -- XMonad.workspaces = generateWorkspaces i,
  handleEventHook = handleTimeout <+> handleEventHook c
  }

-- generate workspace names suitable for this
generateWorkspaces :: Int -> [String]
generateWorkspaces n = [show x ++ show y | x <- [1..n], y <- [1..n]]

-- initialize the state extension
initState :: Int -> X ()
initState i = XS.put $ First i

-- compWS t i f
-- apply f on a workspace id after this function has been run twice.
-- t is a timeout in secounds, if the second button is not pressed
-- before the timeout, then the workspace that is chosen is the one
-- accociated with the key combination 0i.
-- finally, i is the unique id for this button.
-- example:
-- [((modm .|. m, k), compWS (1/5) i (windows . f)) |
--   (i, k) <- zip [0..] [xK_1 .. xK_3],
--   (f, m) <- [(W.greedyView, 0)]]
compWS :: Rational -> CompInd -> (WorkspaceId -> X ()) -> X ()
compWS timeout key f = do
  state <- XS.get
  case state of
    First n             -> startTimer timeout >>= (\id -> XS.put $ Second n key id f)
    -- FIXME: vad händer om f:ena inte är samma, hmm....
    Second n oldKey _ _ -> runSecond n oldKey key f

handleTimeout :: Event -> X All
handleTimeout e = do
  state <- XS.get
  case state of
    First _           -> return (All True)
    Second n key id f -> handleTimer id e (runSecond n 0 key f >> return Nothing) >> return (All True)

-- extracted common functionality for handleTimeout and compWS
runSecond :: Int -> CompInd -> CompInd -> (WorkspaceId -> X ()) -> X ()
runSecond n oldKey key f = do
  ws <- XMonad.workspaces . config <$> ask
  XS.put (First n) >> f (ws !! (n * oldKey + key))

