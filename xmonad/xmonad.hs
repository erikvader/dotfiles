{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures #-}

import System.Exit

import Codec.Binary.UTF8.String as UTF8

import XMonad hiding ( (|||) )
import XMonad.Config.Desktop

import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.Warp

import XMonad.Util.SpawnOnce

import XMonad.Prompt.ConfirmPrompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import qualified XMonad.Layout.Dwindle as Dwind
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Gaps
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig

import Erik.Spacing
import Erik.MyStuff
import Erik.MyLimitWindows
-- import XMonad.Layout.LimitWindows

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Maybe

import qualified DBus as D
import qualified DBus.Client as D

myModMask = mod4Mask

myWorkspaces = ["1 \62056", "2 \61508"] ++ map ((++ " \61705") . show) [3..9]

myBaseLayouts = Tall 1 (3/100) (1/2) ||| renamed [Replace "OneBig"] (OneBig (3/4) (3/4)) ||| ThreeColMid 1 (3/100) (1/2) ||| mosaic 1.1 [3,2,2] ||| Grid ||| renamed [Replace "Spiral"] (Dwind.Spiral Dwind.R Dwind.CW 1.4 1.1)
myBaseLayoutsNames = ["Tall", "OneBig", "ThreeCol", "Mosaic", "Grid", "Spiral"]

lwLimit :: Int
lwLimit = 3

myLayoutHook =
  limitWindows lwLimit False True $
  renamed [CutWordsLeft 2] $ -- remove smartspacing text
  smartSpacing 3 .
  mkToggle (single MIRROR) $
  myBaseLayouts

myStartupHook =
  spawnOnce "pulseaudio" <+>
  spawnOnce "pa-applet" <+>
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" <+>
  spawn "$HOME/.i3/display_updater.sh" <+>
  spawnOnce "compton -b" <+>
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
  spawnOnce "redshift-gtk" <+>
  spawnOnce "blueman-applet" <+>
  spawnOnce "google-chrome-stable" <+>
  spawnOnce "emacs --daemon" <+>
  initStates myWorkspaces lwLimit False True -- limitWindows

myUpdatePointer = updatePointer (0.5, 0.5) (0.25, 0.25)

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [
    -- limitWindows
    ((modm, xK_y), decreaseLimit),
    ((modm, xK_e), increaseLimit),
    ((modm, xK_c), toggleLimit),
    ((modm, xK_f), toggleFull),

    --cycle
    -- ((modm, xK_i), onLayout [("TwoPane", rotFocusedUp)] rotAllUp), --rotate current window in two pane pretty much
    -- ((modm, xK_u), onLayout [("TwoPane", rotFocusedDown)] rotAllDown),
    ((modm, xK_i), rotateVisibleUp), --rotate current window in two pane pretty much
    ((modm, xK_u), rotateVisibleDown),
    -- ((modm .|. shiftMask, xK_i), rotUnfocusedUp), --rotate all except the one with focus
    -- ((modm .|. shiftMask, xK_u), rotUnfocusedDown),
    ((modm .|. shiftMask, xK_i), rotateFocHiddenUp),
    ((modm .|. shiftMask, xK_u), rotateFocHiddenDown),
    -- ((modm, xK_z), rotLastUp), -- rotate all windows after, including focused
    ((modm, xK_w), bury),

    -- rofi
    ((modm, xK_x), spawn "rofi -show run"),
    ((modm, xK_Tab), spawn "rofi -show window"),
    ((modm, xK_r), spawn "$HOME/.i3/rofi_script_selector.sh"),
    ((modm .|. shiftMask, xK_r), spawn "rofi -show drun"),

    -- printscreen
    ((0, xK_Print), spawn "i3-scrot"),
    ((modm, xK_Print), spawn "i3-scrot -w"),
    ((modm .|. shiftMask, xK_Print), spawn "i3-scrot -s"),
    ((controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh"),
    ((modm .|. controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh -u"),
    ((modm .|. shiftMask .|. controlMask, xK_Print), spawn "$HOME/.i3/scrot_clipboard.sh -s"),
    -- flash
    -- ((modm, xK_minus), spawn "$HOME/.i3/flasher.sh"),

    ((modm, xK_z), pointerDance 15 60000),

    -- toggle prog mode
    ((modm, xK_m), spawn "$HOME/prog_mode_toggle.sh"),
    ((modm .|. shiftMask, xK_m), spawn "$HOME/prog_mode_toggle.sh swetoggle"),

    -- display stuff
    ((modm, xK_plus), spawn "$HOME/.i3/display_updater.sh"),
    ((modm, xK_grave), spawn "pkill -USR1 '^redshift$'"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),

    -- gaps
    ((modm, xK_bracketleft), incSpacing (-1)),
    ((modm, xK_bracketright), incSpacing 1),
    ((modm, xK_at), setSpacing 0),

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
    ((modm, xK_k), windows W.focusUp >> myUpdatePointer),

    ((modm, xK_j), windows W.focusDown >> myUpdatePointer),

    -- Move focus to the master window
    ((modm, xK_b), windows W.focusMaster >> myUpdatePointer),

    -- Swap the focused window and the master window
    ((modm .|. shiftMask, xK_b), windows W.swapMaster >> myUpdatePointer),

    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows W.swapDown >> myUpdatePointer),

    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows W.swapUp >> myUpdatePointer),

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

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    ((modm .|. shiftMask, xK_0), confirmPrompt def "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt def "power off?" $ spawn "poweroff"),

    -- Restart xmonad
    ((modm .|. shiftMask, xK_c), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows (f i) >> myUpdatePointer)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    [((modm .|. controlMask, k), sendMessage $ JumpToLayout l) | (l, k) <- zip myBaseLayoutsNames [xK_1 .. xK_9]]

    ++

    --
    -- mod-{F1,F2,f3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F1,F2,f3}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f) >> myUpdatePointer)
        | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- limitWindows
logWindowCount :: X (Maybe String)
logWindowCount = do
  wc <- stackSize . W.stack . W.workspace . W.current <$> gets windowset
  state <- getCurrentState
  return $ warningNumber wc state
  where
    warningNumber _ Nothing = Just ""
    warningNumber open (Just (hidden, full, off))
      | (full || not off) && actualHidden full > 0 = Just $ wrap "%{F#ff8c00}" "%{F-}" $ show $ actualHidden full
      | otherwise = Just ""
      where actualHidden False = open - hidden
            actualHidden True  = open - 1

-- limitWindows
logWindowStatus :: X (Maybe String)
logWindowStatus = status <$> getCurrentState
  where
    status Nothing              = Just ""
    status (Just (_, True, _))  = Just "Full "
    status (Just (l, _, False)) = Just $ "Limit " ++ show l ++ " "
    status _                    = Just ""

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
-- myLogHook dbus = def { ppOutput = dbusOutput dbus }
myLogHook dbus = def
    { ppOutput = dbusOutput dbus,
    ppCurrent = wrap "%{B#505050 F#dfdfdf U#ffb52a +u}  " "  %{B- F- -u}",
    ppVisible = wrap "  " "  ",
    ppUrgent = wrap "%{B#bd2c40}  " "!  %{B-}",
    ppHidden = wrap "  " "  ",
    ppWsSep = "",
    ppSep = " : ",
    ppTitle = shorten 40,
    ppOrder = \(w:l:t:lwc:lwf:_) -> filter (not . null) [w, lwf ++ l, lwc, t],
    ppExtras = [logWindowCount, logWindowStatus]
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

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
  startupHook = startupHook baseConfig <+> myStartupHook
  }

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $ ewmh $ myConfig {
    logHook = logHook myConfig <+> dynamicLogWithPP (myLogHook dbus),
    handleEventHook = handleEventHook myConfig <+> fullscreenEventHook
    }

