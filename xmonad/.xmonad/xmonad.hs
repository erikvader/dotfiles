{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

import System.Posix.Types (CMode(..))
import System.Posix.IO (dupTo,closeFd,createFile,stdError)
import Control.Exception (catch,SomeException)
import System.Directory (doesFileExist,removeFile,executable,getPermissions,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit
import Control.Monad (when)
import Data.List

import Graphics.X11.ExtraTypes.XF86

import Codec.Binary.UTF8.String as UTF8

import XMonad hiding ( (|||) )
import XMonad.Config.Desktop

import XMonad.Actions.Warp
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.PhysicalScreens
-- import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WorkspaceNames

import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

import XMonad.Prompt.ConfirmPrompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory

import qualified XMonad.Layout.Dwindle as Dwind
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Layout.Spacing

import Erik.MyStuff
import Erik.IndiPP
import qualified Erik.MyLimitWindows as L
import Erik.ThreeColP

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as D

myModMask = mod4Mask

scratchWS = "\61485"

myWorkspaces = ["1 \62056", "2 \61508"] ++ map ((++ " \61705") . show) [3..9 :: Integer] ++ [scratchWS]

myBaseLayouts = onWorkspace scratchWS grid tall |||
                ThreeColMid 1 (3/100) (1/3) (1/2) |||
                onWorkspace scratchWS tall grid |||
                GV.SplitGrid GV.L 1 1 (1/2) (16/9) (3/100) |||
                renamed [Replace "Spiral"] (Dwind.Spiral Dwind.R Dwind.CW 1.4 1.1)
  where grid = GV.Grid 1
        tall = Tall 1 (3/100) (1/2)

myBaseLayoutsNames = ["Tall", "ThreeCol", "Grid", "SplitGrid", "Spiral"]

myLayoutHook =
  L.limitWindows 2 False True $
  renamed [CutWordsLeft 1] $ -- remove smartspacing text
  spacingRaw True (Border 3 3 3 3) True (Border 3 3 3 3) True $
  mkToggle (single MIRROR)
  myBaseLayouts

myStartupHook = runXmonadStartupOnce

runXmonadStartupOnce :: X ()
runXmonadStartupOnce = do
  home <- io getHomeDirectory
  let startupFile = home </> ".xmonad_startup"
  itExists <- io $ doesFileExist startupFile
  when itExists $ do
    perms <- io $ getPermissions startupFile
    when (executable perms) $
      spawnOnce startupFile

-- if not on scratch -> greedyView it
-- if     on scratch -> view latest workspace that is not visible on any screen
scratchVisit :: X ()
scratchVisit = gets windowset >>= (\ss -> func $ map (W.tag . W.workspace) (W.current ss : W.visible ss))
  where
    func [] = return ()
    func (cur:vis) | cur == scratchWS = do
                       hist <- workspaceHistory
                       let cand = find (\x -> x `notElem` (scratchWS:vis)) hist
                       whenJust cand gw
                   | otherwise = gw scratchWS
    gw x = windows $ W.greedyView x

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [
    -- limitWindows
    ((modm, xK_y), L.decreaseLimit),
    ((modm, xK_e), L.increaseLimit),
    ((modm, xK_c), L.toggleLimit),
    ((modm, xK_f), L.toggleFull),
    ((modm .|. shiftMask, xK_f), sendMessage $ Toggle MIRROR),

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

    ((modm, xK_o), windowsLowestEmpty W.view $ XMonad.workspaces conf),
    ((modm .|. shiftMask, xK_o), windowsLowestEmpty shiftView $ XMonad.workspaces conf),

    -- rofi
    ((modm, xK_x), spawn "rofi -show run"),
    ((modm .|. shiftMask, xK_x), spawn "rofi -show drun"),
    ((modm, xK_Escape), spawn "rofi -show window"),
    ((modm, xK_r), spawn "rofi_script_selector"),
    ((modm .|. shiftMask, xK_r), spawn "open_downloaded_pdf"),

    -- screens
    ((modm, xK_Tab), onNextNeighbour def W.view),
    ((modm .|. shiftMask, xK_Tab), onPrevNeighbour def W.view),
    ((modm .|. controlMask, xK_j), onNextNeighbour def W.view),
    ((modm .|. controlMask, xK_k), onPrevNeighbour def W.view),
    ((modm .|. controlMask .|. shiftMask, xK_j), onNextNeighbour def swapWith),
    ((modm .|. controlMask .|. shiftMask, xK_k), onPrevNeighbour def swapWith),

    -- printscreen
    ((0, xK_Print), spawn "maim_clipboard -su"),

    -- flash
    ((modm, xK_z), spawn "flasher"),
    ((modm .|. shiftMask, xK_z), spawn "mouse_dance"),

    -- toggle prog mode
    ((modm .|. shiftMask, xK_m), spawn "prog_mode_toggle"),
    ((modm, xK_m), spawn "prog_mode_toggle swetoggle"),

    -- display stuff
    ((modm, xK_plus), spawn "display_updater all"),
    ((modm .|. shiftMask, xK_plus), spawn "xrandr --output eDP1 --auto"),
    ((modm, xK_grave), spawn "pkill -f -USR1 redshift-manual-daemon"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),
    ((modm, xK_asciicircum), spawn "pkill compton"),

    ((modm, xK_minus), spawn "theme_select safe"),
    ((modm .|. shiftMask, xK_minus), spawn "theme_select -r"),

    -- gaps
    ((modm, xK_bracketleft), decScreenWindowSpacing 1),
    ((modm, xK_bracketright), incScreenWindowSpacing 1),
    ((modm, xK_at), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled),

    ((modm, xK_Left), spawn "i3_brightness -steps 1 -dec 1"),
    ((modm, xK_Right), spawn "i3_brightness -steps 1 -inc 1"),
    ((0, xF86XK_MonBrightnessUp), spawn "i3_brightness -steps 1 -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "i3_brightness -steps 1 -dec 10"),

    -- media buttons
    ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
    ((0, xF86XK_AudioNext), spawn "playerctl next"),
    ((0, xF86XK_AudioPrev), spawn "playerctl previous"),

    -- launch a terminal
    ((modm .|. mod1Mask, xK_Return), spawn $ XMonad.terminal conf),
    ((modm, xK_Return), spawn "urxvt"),
    ((modm .|. controlMask, xK_Return), spawn "urxvt -e zsh -is eval ranger-cd"),
    ((modm .|. shiftMask, xK_Return), spawn "emacsclient -nc"),
    -- ((modm, xK_BackSpace), spawn "$HOME/.emacs_anywhere/bin/run"),

    -- toggle zoom
    -- ((modm, xK_f), sendMessage $ Toggle FULL),

    -- close focused window
    ((modm, xK_q), kill),

     -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),

    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),

    -- Resize viewed windows to the correct size
    ((modm, xK_n), refresh),

    ((modm, xK_a), warpToWindow 1 1),
    ((modm .|. shiftMask, xK_a), warpToWindow 0.5 0.5),
    ((modm .|. controlMask, xK_a), myUpdatePointerToggle),

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

    -- focus next or previous workspace
    ((modm, xK_period), nextWS),
    ((modm, xK_comma), prevWS),

    -- moves workspaces up or down
    ((modm .|. shiftMask, xK_period), swapTo Next),
    ((modm .|. shiftMask, xK_comma), swapTo Prev),

    -- rename workspaces
    ((modm, xK_v), renameWorkspace def),
    ((modm .|. shiftMask, xK_v), setCurrentWorkspaceName ""),
    ((modm .|. mod1Mask, xK_v), mapWorkspaces $ flip setWorkspaceName ""),

    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),

    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),

    -- mosaic
    -- ((modm, xK_s), sendMessage Reset),

    -- Increment the number of windows in the master area
    ((modm .|. shiftMask, xK_h), onLayout [("SplitGrid", sendMessage $ GV.IncMasterRows 1)] (sendMessage (IncMasterN 1))),

    -- Deincrement the number of windows in the master area
    ((modm .|. shiftMask, xK_l), onLayout [("SplitGrid", sendMessage $ GV.IncMasterRows (-1))] (sendMessage (IncMasterN (-1)))),

    ((modm .|. controlMask, xK_h), onLayout [("SplitGrid", sendMessage $ GV.IncMasterCols 1)] (return ())),
    ((modm .|. controlMask, xK_l), onLayout [("SplitGrid", sendMessage $ GV.IncMasterCols (-1))] (return ())),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),
    ((modm .|. shiftMask, xK_t), withFocused $ centerFloat 700 500),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    ((modm, xK_s), sendMessage ToggleStruts),
    -- I want to do this inside xmonad :(
    ((modm .|. shiftMask, xK_s), spawn "xdotool search --name polybar windowmap %@"),
    ((modm .|. controlMask, xK_s), spawn "xdotool search --name polybar windowunmap %@"),

    -- Quit xmonad
    ((modm .|. shiftMask, xK_0), confirmPrompt def "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt def "power off?" $ spawn "poweroff"),

    -- Restart xmonad
    ((modm .|. shiftMask, xK_c), spawn "if xmonad --recompile; then xmonad --restart; notify-send 'XMonad restarted'; else notify-send 'XMonad failed to compile'; fi"),

    ((modm, xK_d), scratchVisit),
    ((modm .|. shiftMask, xK_d), windows $ W.shift scratchWS)
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
        , (f, m) <- [(W.greedyView, 0),
                     (\i -> W.view i . W.shift i, controlMask .|. shiftMask),
                     (W.shift, controlMask)
                     -- , (W.view, shiftMask)
                    ]]

    ++

    -- jump to layout
    [((modm .|. mod1Mask, k), sendMessage $ JumpToLayout l) | (l, k) <- zip myBaseLayoutsNames [xK_1 .. xK_9]]

    ++

    -- move to screen with shift+num instead of Fnum
    [((m .|. modm, key), f sc)
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(viewScreen def, shiftMask)]]

    ++

    --
    -- mod-{F1,F2,f3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F1,F2,f3}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), f sc)
        | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
        , (f, m) <- [(viewScreen def, 0),
                     (\i -> sendToScreen def i >> viewScreen def i, shiftMask .|. controlMask),
                     (\i -> getScreen def i >>= maybe (return Nothing) screenWorkspace >>= flip whenJust (windows . W.greedyView), shiftMask),
                     (sendToScreen def, controlMask)
                    ]]

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

myFocusPPXin :: PP
myFocusPPXin = def
    {
      ppCurrent = wrap "%{B#505050 u#ffb52a +u}[  " "  ]%{B- -u}",
      ppVisible = wrap "%{B#505050}[  " "  ]%{B-}",
      ppUrgent = wrap " %{B#bd2c40} " "! %{B-} ",
      ppHidden = wrap "  " "  ",
      ppWsSep = "",
      ppSep = " %{F#ffb52a}:%{F-} ",
      ppTitle = shorten 60,
      ppSort = getSortByXineramaPhysicalRule def,
      ppOrder = \(w:l:t:lwc:lwf:ldh:_) -> filter (not . null) [w, lwf ++ l, ldh, lwc, t],
      ppExtras = logLimitWindows
    }

myNonfocusPPXin :: PP
myNonfocusPPXin = myFocusPPXin {
  ppCurrent = wrap "%{B#505050 u#00ace6 +u}[  " "  ]%{B- -u}",
  ppSep = " %{F#00ace6}:%{F-} "
  }

-- myFocusPP :: PP
-- myFocusPP = myFocusPPXin
--     {
--       ppCurrent = wrap "%{B#505050 u#ffb52a +u}  " "  %{B- -u}",
--       ppVisible = wrap "%{B#505050 u#00bfff +u}  " "  %{B- -u}",
--       ppSort = ppSort def
--     }

-- myNonfocusPP :: PP
-- myNonfocusPP = myFocusPP {
--   ppCurrent = wrap "%{u#e69500 +u}  " "  %{-u}",
--   ppVisible = wrap "%{u#0086b3 +u}  " "  %{-u}"
--   }

multiPrepare :: D.Client -> String -> PP -> X PP
multiPrepare dbus output pp = do
  L.updateCurrentState
  workspaceNamesPP $ pp {ppOutput = dbusOutput dbus . (output ++) . fixXinerama}
  where
    fixXinerama :: String -> String
    fixXinerama s = removeIndices 0 s $ tail . init $ findIndices (\c -> c == '[' || c == ']') $ takeTo (ppSep pp) s

    takeTo :: Eq a => [a] -> [a] -> [a]
    takeTo [] src = src
    takeTo _ [] = []
    takeTo to src | to `isPrefixOf` src = []
                  | otherwise = head src : takeTo to (tail src)

    removeIndices :: Int -> String -> [Int] -> String
    removeIndices _ [] _ = []
    removeIndices _ ss [] = ss
    removeIndices c (s:ss) (i:is) | c == i    = removeIndices (c+1) ss is
                                  | otherwise = s:removeIndices (c+1) ss (i:is)

baseConfig = desktopConfig {
  modMask = myModMask,
  borderWidth = 0,
  terminal = "urxvt",
  keys = myKeys,
  workspaces = myWorkspaces
  }

myConfig = baseConfig {
  manageHook = composeAll [ isDialog --> doCenterFloat ] <+> manageHook baseConfig,
  startupHook = startupHook baseConfig <+> myStartupHook,
  logHook = logHook baseConfig >> myUpdatePointer
  }

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

errorFile :: FilePath
errorFile = "/tmp/xmonad-error"

main :: IO ()
main = do
  dbus <- D.connectSession
      -- Request access to the DBus name
  _ <- D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  -- custom .xsession-error
  catch (do
            closeFd stdError
            fs <- doesFileExist errorFile
            when fs $ removeFile errorFile
            fd <- createFile errorFile (CMode 0o666)
            _ <- dupTo fd stdError
            return ())
    (\e -> trace (show (e :: SomeException)))

  xmonad $ indiPP $ withUrgencyHook NoUrgencyHook $ ewmh $ docks $ myConfig {
    layoutHook = avoidStruts myLayoutHook,
    handleEventHook = handleEventHook myConfig <+> fullscreenEventHook,
    logHook = logHook myConfig <+> workspaceHistoryHook <+> workspaceNamesClearerLogHook <+> multiPP myFocusPPXin myNonfocusPPXin (multiPrepare dbus)
    }
