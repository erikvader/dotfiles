{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures #-}

import System.Posix.Files (createNamedPipe)
import System.Posix.Types (CMode(..))
import Control.Exception (catch,SomeException)
import System.Directory (doesFileExist)
import System.IO
import System.Exit
import Data.Bits (testBit)
import Control.Monad (unless)
import Data.List

import Codec.Binary.UTF8.String as UTF8

import XMonad hiding ( (|||) )
import XMonad.Config.Desktop

import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Actions.CycleWS (nextScreen, swapNextScreen)

import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

import XMonad.Prompt.ConfirmPrompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import qualified XMonad.Layout.Dwindle as Dwind
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Grid
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig

import Erik.Spacing
import Erik.MyStuff
import qualified Erik.MyLimitWindows as L
-- import XMonad.Layout.LimitWindows

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myModMask = mod4Mask

myWorkspaces = ["1 \62056", "2 \61508"] ++ map ((++ " \61705") . show) [3..9 :: Integer]

myBaseLayouts = Tall 1 (3/100) (1/2) ||| renamed [Replace "OneBig"] (OneBig (3/4) (3/4)) ||| ThreeColMid 1 (3/100) (1/2) ||| mosaic 1.1 [3,2,2] ||| Grid ||| renamed [Replace "Spiral"] (Dwind.Spiral Dwind.R Dwind.CW 1.4 1.1)
myBaseLayoutsNames = ["Tall", "OneBig", "ThreeCol", "Mosaic", "Grid", "Spiral"]

lwLimit :: Int
lwLimit = 2

myLayoutHook =
  L.limitWindows lwLimit False True $
  renamed [CutWordsLeft 2] $ -- remove smartspacing text
  smartSpacing 3 .
  mkToggle (single MIRROR) $
  myBaseLayouts

myStartupHook =
  spawnOnce "pulseaudio" <+>
  spawnOnce "pa-applet" <+>
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" <+>
  spawnOnce "display_updater startup" <+>
  -- spawnOnce "compton -b" <+>
  spawnOnce "nm-applet" <+>
  spawnOnce "xfce4-power-manager" <+>
  spawnOnce "pamac-tray" <+>
  spawnOnce "clipit" <+>
  spawn "ff-theme-util" <+>
  spawn "fix_xcursor" <+>
  spawnOnce "echo off > $HOME/.program_mode" <+>
  spawnOnce "$HOME/prog_mode_toggle.sh on" <+>
  -- set mouse speed
  spawn "xinput --set-prop 'ELAN0501:00 04F3:3060 Touchpad' 'libinput Accel Speed' 0.9" <+>
  -- scrolla in other direction
  spawn "xinput --set-prop 'ELAN0501:00 04F3:3060 Touchpad' 'libinput Natural Scrolling Enabled' 1" <+>
  spawnOnce "dropbox start" <+>
  -- spawnOnce "redshift-gtk" <+>
  -- spawnOnce "blueman-applet" <+>
  spawnOnce "google-chrome-stable" <+>
  spawnOnce "emacs --daemon"

-- Do the same thing as XMonad.Actions.UpdatePointer, except that it
-- also checks whether a mouse button is currently pressed. If one is
-- pressed, then that probably means that something is being dragged,
-- and if something is being dragged we don't want the cursor to jump
-- all over the place. So if a mouse button is pressed, this does
-- nothing.
myUpdatePointer = do
  dpy <- asks display
  root <- asks theRoot
  (_,_,_,_,_,_,_,m) <- io $ queryPointer dpy root
  unless (testBit m 9 || testBit m 8 || testBit m 10) $
    updatePointer (0.5, 0.5) (0.25, 0.25)

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [
    -- limitWindows
    ((modm, xK_y), L.decreaseLimit),
    ((modm, xK_e), L.increaseLimit),
    ((modm, xK_c), L.toggleLimit),
    ((modm, xK_f), L.toggleFull),

    --cycle
    -- ((modm, xK_i), onLayout [("TwoPane", rotFocusedUp)] rotAllUp), --rotate current window in two pane pretty much
    -- ((modm, xK_u), onLayout [("TwoPane", rotFocusedDown)] rotAllDown),
    ((modm, xK_i), L.rotateVisibleUp), --rotate current window in two pane pretty much
    ((modm, xK_u), L.rotateVisibleDown),
    -- ((modm .|. shiftMask, xK_i), rotUnfocusedUp), --rotate all except the one with focus
    -- ((modm .|. shiftMask, xK_u), rotUnfocusedDown),
    ((modm .|. shiftMask, xK_i), L.rotateFocHiddenUp),
    ((modm .|. shiftMask, xK_u), L.rotateFocHiddenDown),
    -- ((modm, xK_z), rotLastUp), -- rotate all windows after, including focused
    ((modm, xK_w), L.bury),

    ((modm, xK_o), focusLowestEmpty $ XMonad.workspaces conf),

    -- rofi
    ((modm, xK_x), spawn "rofi -show run"),
    ((modm .|. shiftMask, xK_x), spawn "rofi -show drun"),
    ((modm, xK_Escape), spawn "rofi -show window"),
    ((modm, xK_r), spawn "$HOME/.i3/rofi_script_selector.sh"),
    ((modm .|. shiftMask, xK_r), spawn "display_updater_rofi"),

    -- screens
    ((modm, xK_Tab), nextScreen),
    ((modm .|. shiftMask, xK_Tab), swapNextScreen),

    -- printscreen
    ((0, xK_Print), spawn "i3-scrot"),
    ((modm, xK_Print), spawn "i3-scrot -w"),
    ((modm .|. shiftMask, xK_Print), spawn "i3-scrot -s"),
    ((controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh"),
    ((modm .|. controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh -u"),
    ((modm .|. shiftMask .|. controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh -s"),
    -- flash
    -- ((modm, xK_minus), spawn "$HOME/.i3/flasher.sh"),

    ((modm, xK_z), spawn "mouse_dance"),

    -- toggle prog mode
    ((modm .|. shiftMask, xK_m), spawn "$HOME/prog_mode_toggle.sh"),
    ((modm, xK_m), spawn "$HOME/prog_mode_toggle.sh swetoggle"),

    -- display stuff
    ((modm, xK_plus), spawn "display_updater update"),
    ((modm, xK_grave), spawn "pgrep -x redshift && pkill -USR1 -x redshift || redshift-gtk"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),
    ((modm, xK_asciicircum), spawn "pkill compton"),

    ((modm, xK_minus), spawn "theme_select safe"),
    ((modm .|. shiftMask, xK_minus), spawn "theme_select -r"),

    -- gaps
    ((modm, xK_bracketleft), incSpacing (-1)),
    ((modm, xK_bracketright), incSpacing 1),
    ((modm, xK_at), setSpacing 0),

    ((modm, xK_Left), spawn "i3_brightness -dec 1"),
    ((modm, xK_Right), spawn "i3_brightness -inc 1"),

    -- launch a terminal
    ((modm .|. mod1Mask, xK_Return), spawn $ XMonad.terminal conf),
    ((modm, xK_Return), spawn "urxvt"),
    ((modm .|. shiftMask, xK_Return), spawn "emacsclient -nc"),
    ((modm, xK_BackSpace), spawn "$HOME/.emacs_anywhere/bin/run"),

    -- toggle zoom
    -- ((modm, xK_f), sendMessage $ Toggle FULL),

    ((modm, xK_d), sendMessage $ Toggle MIRROR),

    -- close focused window
    ((modm, xK_q), kill),

     -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),

    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),

    -- Resize viewed windows to the correct size
    ((modm, xK_n), refresh),

    ((modm .|. shiftMask, xK_a), warpToWindow 1 1),
    ((modm, xK_a), warpToWindow 0.5 0.5),

    -- Move focus to the previous window
    ((modm, xK_k), windows W.focusUp),

    ((modm, xK_j), windows W.focusDown),

    -- Move focus to the master window
    ((modm, xK_b), windows W.focusMaster),

    -- Swap the focused window and the master window
    ((modm .|. shiftMask, xK_b), windows W.swapMaster),

    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows W.swapDown),

    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows W.swapUp),

    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),

    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),

    -- mosaic
    ((modm, xK_s), sendMessage Reset),

    -- Increment the number of windows in the master area
    ((modm .|. shiftMask , xK_h), onLayout [("Mosaic", sendMessage Taller)] (sendMessage (IncMasterN 1))),

    -- Deincrement the number of windows in the master area
    ((modm .|. shiftMask , xK_l), onLayout [("Mosaic", sendMessage Wider)] (sendMessage (IncMasterN (-1)))),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),
    -- ((modm .|. shiftMask, xK_t), withFocused $ windows . (\a b -> W.float a (W.RationalRect 100 100 100 100) b)),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    ((modm .|. shiftMask, xK_0), confirmPrompt def "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt def "power off?" $ spawn "poweroff"),

    -- Restart xmonad
    ((modm .|. shiftMask, xK_c), spawn "xmonad --recompile && (xmonad --restart; notify-send 'XMonad restarted') || notify-send 'Failed to compile'")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (\i -> W.view i . W.shift i, controlMask .|. shiftMask), (W.shift, controlMask), (W.greedyView, shiftMask)]]
    ++

    [((modm .|. mod1Mask, k), sendMessage $ JumpToLayout l) | (l, k) <- zip myBaseLayoutsNames [xK_1 .. xK_9]]

    ++

    --
    -- mod-{F1,F2,f3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F1,F2,f3}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
        , (f, m) <- [(W.view, 0), (\i -> W.view i . W.shift i, shiftMask .|. controlMask), (W.greedyView, shiftMask), (W.shift, controlMask)]]

logLimitWindows :: [X (Maybe String)]
logLimitWindows =
  map (<$> L.getCurrentState) [windowCount, status, detach]
  where
    status L.LimitState{L.sfull=True}             = Just "%{F#eeee00}Full %{F-}"
    status L.LimitState{L.slimit=l, L.soff=False} = Just $ "%{F#eeee00}Limit " ++ show l ++ "%{F-} "
    status _                                      = Just ""

    windowCount L.LimitState{L.sfull=full, L.soff=off, L.shidden=hidden}
      | (full || not off) && hidden > 0 = Just $ wrap "%{F#ff8c00}" "%{F-}" $ show hidden
      | otherwise = Just ""

    detach L.LimitState{L.sdetachedOffset=det, L.sfull=full, L.soff=off}
      | (full || not off) && det > 0 = Just $ wrap "%{F#ff00ff}" "%{F-}" "d"
      | otherwise = Just ""

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: Maybe Handle -> PP
myLogHook mhandle = def
    { ppOutput = pipeOutput mhandle . fixXinerama,
      ppCurrent = wrap "%{B#505050 U#ffb52a +u}[  " "  ]%{B- -u}",
      ppVisible = wrap "%{B#505050}[  " "  ]%{B-}",
      ppUrgent = wrap "%{B#bd2c40}  " "!  %{B-}",
      ppHidden = wrap "  " "  ",
      ppWsSep = "",
      ppSep = " : ",
      ppTitle = shorten 60,
      ppSort = getSortByXineramaRule,
      ppOrder = \(w:l:t:lwc:lwf:ldh:_) -> filter (not . null) [w, lwf ++ l, ldh, lwc, t],
      ppExtras = logLimitWindows
    }
  where
    fixXinerama :: String -> String
    fixXinerama s = removeIndices 0 s $ tail . init $ findIndices (\c -> c == '[' || c == ']') $ takeWhile (/= ':') s

    removeIndices :: Int -> String -> [Int] -> String
    removeIndices _ [] _ = []
    removeIndices _ ss [] = ss
    removeIndices c (s:ss) (i:is) | c == i    = removeIndices (c+1) ss is
                                  | otherwise = s:removeIndices (c+1) ss (i:is)

pipeOutput :: Maybe Handle -> String -> IO ()
pipeOutput Nothing _ = return ()
pipeOutput (Just h) s = hPutStrLn h (UTF8.decodeString s) >> hFlush h

baseConfig = desktopConfig {
  modMask = myModMask,
  borderWidth = 0,
  terminal = "termite",
  keys = myKeys,
  workspaces = myWorkspaces
  }

myConfig = baseConfig {
  layoutHook = avoidStruts myLayoutHook,
  manageHook = composeAll [ isDialog --> doCenterFloat ] <+> manageDocks <+> manageHook baseConfig,
  startupHook = startupHook baseConfig <+> myStartupHook,
  logHook = logHook baseConfig >> myUpdatePointer
  }

pipeName :: FilePath
pipeName = "/tmp/XMonadLog"

main :: IO ()
main = do
  -- create pipe
  mhandle <- catch (do
                       fs <- doesFileExist pipeName
                       if fs
                         then return ()
                         else createNamedPipe pipeName (CMode 0o666)
                       Just <$> openFile pipeName ReadWriteMode)
               (\e -> do
                        trace (show (e :: SomeException))
                        return Nothing)

  xmonad $ ewmh $ myConfig {
    logHook = logHook myConfig <+> L.updateCurrentState <+> dynamicLogWithPP (myLogHook mhandle),
    handleEventHook = handleEventHook myConfig <+> fullscreenEventHook
    }

