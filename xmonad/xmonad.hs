import System.IO
import System.Exit

import Codec.Binary.UTF8.String as UTF8

import XMonad hiding ( (|||) )
import XMonad.Config.Desktop

import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Dwindle
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import Erik.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as D

myModMask = mod4Mask

-- myWorkspaces = ["  ", "2"] ++ map (\n -> show n ++ "g") [3..9]
myWorkspaces = map show [1..9]

myBaseLayouts = Tall 1 (3/100) (1/2) ||| TwoPane (3/100) (1/2) ||| renamed [Replace "Spiral"] (Spiral R CW 1.4 1.1) ||| Grid
myBaseLayoutsNames = ["Tall", "TwoPane", "Spiral", "Grid"]

myLayoutHook =
  gaps [(L, 3), (R, 3)] . -- compensate for weird spacing at the edges
  smartSpacing 3 .
  mkToggle (single FULL) .
  mkToggle (single MIRROR) $
  myBaseLayouts

myStartupHook =
  spawnOnce "pulseaudio" <+>
  spawnOnce "pa-applet" <+>
  -- spawn "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" <+>
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
  spawnOnce "emacs --daemon"

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [

    --cycle
    ((modm, xK_i), rotUnfocusedUp),
    ((modm, xK_u), rotUnfocusedDown),
    ((modm .|. shiftMask, xK_i), rotFocusedUp),
    ((modm .|. shiftMask, xK_u), rotFocusedDown), -- rotAllDown ??

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
    ((modm, xK_minus), spawn "$HOME/.i3/flasher.sh"),

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
    ((modm, xK_Return), spawn $ XMonad.terminal conf),
    ((modm .|. shiftMask, xK_Return), spawn "emacsclient -nc"),

    -- toggle zoom
    ((modm, xK_f), sendMessage $ Toggle FULL),

    ((modm, xK_d), sendMessage $ Toggle MIRROR),

    -- close focused window
    ((modm, xK_q), kill),

     -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),

    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),

    -- Resize viewed windows to the correct size
    ((modm, xK_n), refresh),

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

    -- Increment the number of windows in the master area
    ((modm .|. shiftMask , xK_h), sendMessage (IncMasterN 1)),

    -- Deincrement the number of windows in the master area
    ((modm .|. shiftMask , xK_l), sendMessage (IncMasterN (-1))),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    ((modm, xK_0), confirmPrompt def "exit" $ io exitSuccess),

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
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((modm .|. controlMask, k), sendMessage $ JumpToLayout l) | (l, k) <- zip myBaseLayoutsNames [xK_1 .. xK_9]]

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

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
    ppLayout = unwords . (\x -> drop 2 x) . words,
    ppTitle = shorten 40
    -- ppOrder = \(w:_:t:rest) -> w:t:rest
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
  manageHook = manageDocks <+> manageHook baseConfig,
  startupHook = startupHook baseConfig <+> myStartupHook
  }

main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ myConfig {
      logHook = logHook myConfig <+> dynamicLogWithPP (myLogHook dbus) >> updatePointer (0.95, 0.95) (0, 0)
      }

