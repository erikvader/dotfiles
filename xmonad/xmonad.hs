import System.IO
import System.Exit

import Codec.Binary.UTF8.String as UTF8

import XMonad
import XMonad.Config.Desktop

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

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as D

myModMask = mod4Mask

myLayoutHook =
  smartBorders .
  mkToggle (single FULL) .
  mkToggle (single MIRROR) $
  Tall 1 (3/100) (1/2) ||| Spiral R CW 1.5 1.1

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
  spawnOnce "blueman-applet"

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
  [

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

    -- display stuff
    ((modm, xK_plus), spawn "$HOME/.i3/display_updater.sh"),
    ((modm, xK_grave), spawn "pkill -USR1 '^redshift$'"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),

    -- launch a terminal
    ((modm, xK_Return), spawn $ XMonad.terminal conf),

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
    ((modm, xK_0), confirmPrompt defaultXPConfig "exit" $ io (exitWith ExitSuccess)),

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
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#504945"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
-- myLogHook dbus = def { ppOutput = dbusOutput dbus }
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = shorten 40
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
  keys = myKeys
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

    xmonad $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

