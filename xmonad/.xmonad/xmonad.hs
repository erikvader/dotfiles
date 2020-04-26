{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP, PartialTypeSignatures #-}

import System.Posix.Types (CMode(..))
import System.Posix.IO (dupTo,closeFd,createFile,stdError)
import Control.Exception (catch,SomeException,IOException)
import System.Directory (doesFileExist,removeFile,executable,getPermissions,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit
import Control.Monad (when,join)
import Data.List
import Data.Maybe (maybeToList,fromMaybe)
import Data.Char (toLower)

import Graphics.X11.ExtraTypes.XF86

import XMonad hiding ( (|||) )

import XMonad.Actions.Warp
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WorkspaceNames

import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Cursor

import XMonad.Prompt.ConfirmPrompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicProperty

import qualified XMonad.Layout.Dwindle as Dwind
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat

import Erik.MyStuff
import Erik.IndiPP
import qualified Erik.MyLimitWindows as L
import Erik.ThreeColP
import Erik.DoubleMaster

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Prompt

myXPConfig :: XPConfig
myXPConfig = def {
  font = "xft:DejaVu Sans:pixelsize=12"
  }

myModMask = mod4Mask

statusbarFifo = "/tmp/statusbar_fifo"

scratchWS = "S"

myWorkspaces = map show [1..(9 :: Int)] ++ [scratchWS]

  --TODO: simplestfloat type
#define COMMA ,
#define GRID GV.Grid (16/10)
#define TALL Tall 1 (3/100) (1/2)
#define TEMPLATE(X,Y,F1,F2) F1 X F2 \
                            F1 DoubleMaster (2/3) (1/2) F2 \
                            F1 (simplestFloat :: _ Window) F2 \
                            F1 Y F2 \
                            F1 ThreeColMid 1 (3/100) (1/3) (1/2) F2 \
                            F1 GV.SplitGrid GV.L 1 1 (1/2) (16/9) (3/100) F2 \
                            F1 renamed [Replace "Spiral"] (Dwind.Spiral Dwind.R Dwind.CW 1.4 1.1)
#define LAYOUTS(X,Y) (TEMPLATE(X,Y,,|||))
#define NAMES(X,Y) TEMPLATE(X,Y,description $,COMMA)

myBaseLayouts = onWorkspace scratchWS LAYOUTS(GRID,TALL) LAYOUTS(TALL,GRID)

#ifndef __HLINT__
myBaseLayoutsNames = [NAMES(TALL,GRID)]
#endif

myLayoutHook =
  L.limitWindows 2 False True $
  renamed [CutWordsLeft 1] $ -- remove smartspacing text
  spacingRaw True (Border 3 3 3 3) True (Border 3 3 3 3) True $
  mkToggle (single MIRROR)
  myBaseLayouts

myStartupHook = runXmonadStartupOnce <+> setDefaultCursor xC_left_ptr

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
    ((modm, xK_w), sendMessage $ Toggle MIRROR),

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
    -- ((modm, xK_w), L.bury),

    ((modm, xK_o), windowsLowestEmpty W.view $ XMonad.workspaces conf),
    ((modm .|. shiftMask, xK_o), windowsLowestEmpty shiftView $ XMonad.workspaces conf),
    ((modm .|. controlMask, xK_o), windowsLowestEmpty W.shift $ XMonad.workspaces conf),

    -- rofi
    ((modm, xK_x), spawn "fzf_run"),
    ((modm, xK_Escape), spawn "fzf_window_switcher"),
    ((modm, xK_r), spawn "rofi_script_selector"),
    ((modm .|. shiftMask, xK_r), spawn "open_downloaded_pdf"),

    -- screens
    ((modm, xK_Tab), switchScreen def tabForward),
    ((modm .|. shiftMask, xK_Tab), switchScreen def tabBackward),
    ((modm .|. controlMask, xK_j), onNextNeighbour def W.view),
    ((modm .|. controlMask, xK_k), onPrevNeighbour def W.view),
    ((modm .|. controlMask .|. shiftMask, xK_j), onNextNeighbour def swapWith),
    ((modm .|. controlMask .|. shiftMask, xK_k), onPrevNeighbour def swapWith),
    ((modm .|. controlMask, xK_b), screenWorkspace 0 >>= flip whenJust (windows . W.view)),

    -- printscreen
    ((0, xK_Print), spawn "maim_clipboard -su"),
    ((shiftMask, xK_Print), spawn "maim-notify -u"),

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
    ((modm .|. shiftMask, xK_grave), spawn "pkill -f -USR2 redshift-manual-daemon"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),
    ((modm, xK_asciicircum), spawn "pkill picom"),

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
    ((modm, xK_Return), spawn "st"),
    ((modm .|. controlMask, xK_Return), spawn "st -e zsh -is eval ranger-cd"),
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
    ((modm, xK_v), renameWorkspace myXPConfig),
    ((modm .|. shiftMask, xK_v), setCurrentWorkspaceName ""),
    ((modm .|. mod1Mask, xK_v), mapWorkspaces $ flip setWorkspaceName ""),

    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),

    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),

    -- mosaic
    -- ((modm, xK_s), sendMessage Reset),

    -- Increment the number of windows in the master area
    ((modm .|. shiftMask, xK_h), onLayout [
          ("SplitGrid", sendMessage $ GV.IncMasterRows 1),
          ("DoubleMaster", sendMessage MasterShrink)
        ]
      (sendMessage (IncMasterN 1))),

    -- Deincrement the number of windows in the master area
    ((modm .|. shiftMask, xK_l), onLayout [
          ("SplitGrid", sendMessage $ GV.IncMasterRows (-1)),
          ("DoubleMaster", sendMessage MasterExpand)
        ]
      (sendMessage (IncMasterN (-1)))),

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
    ((modm .|. shiftMask, xK_s), toggleMapStruts),

    -- Quit xmonad
    ((modm .|. shiftMask, xK_0), confirmPrompt myXPConfig "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt myXPConfig "power off?" $ spawn "poweroff"),

    -- Restart xmonad
    ((modm .|. shiftMask, xK_c), spawn "if xmonad --recompile; then xmonad --restart; notify-send 'XMonad restarted'; else notify-send 'XMonad failed to compile'; fi")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_d])
        , (f, m) <- [(W.greedyView, 0),
                     (shiftView, shiftMask),
                     (W.shift, controlMask)
                    ]]

    ++

    -- NOTE: overwrite greedyView from above
    [((modm, xK_d), scratchVisit)]

    ++

    -- jump to layout
    [((modm .|. mod1Mask, k), sendMessage $ JumpToLayout l) | (l, k) <- zip myBaseLayoutsNames [xK_1 .. xK_9]]

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
  where
    tabForward 3 2 = 1
    tabForward n x = (x+1) `mod` n

    tabBackward 3 _ = 0
    tabBackward n x = (x-1) `mod` n

logLimitWindows :: [X (Maybe String)]
logLimitWindows =
  map (<$> L.getCurrentState) [windowCount, status, detach]
  where
    status L.LimitState{L.sfull=True}             = Just "^fg(yellow2)Full ^fg()"
    status L.LimitState{L.slimit=l, L.soff=False} = Just $ "^fg(yellow2)Limit " ++ show l ++ "^fg() "
    status _                                      = Just ""

    windowCount L.LimitState{L.sfull=full, L.soff=off, L.shidden=hidden}
      | (full || not off) && hidden > 0 = Just $ wrap "^fg(darkorange)" "^fg()" $ show hidden
      | otherwise = Just ""

    detach L.LimitState{L.sdetachedOffset=det, L.sfull=full, L.soff=off}
      | (full || not off) && det > 0 = Just $ wrap "^fg(magenta)" "^fg()" "d"
      | otherwise = Just ""

myFocusPPXin :: PP
myFocusPPXin = def
    {
      ppCurrent = wrap "^bg(lightbg)[  ^fg(orange)" "  ]^bg()",
      ppVisible = wrap "^bg(lightbg)[  ^fg(white)" "  ]^bg()",
      ppUrgent = wrap " ^bg(urgent) ^fg(white)" "! ^bg() ",
      ppHidden = wrap " ^fg(white)" " ",
      ppWsSep = "",
      ppSep = " ^fg(orange):^fg() ",
      ppTitle = shorten 60,
      ppLayout = colorLayout ["Mirror"],
      ppSort = getSortByXineramaPhysicalRule def,
      ppOrder = \(w:l:t:lwc:lwf:ldh:_) -> filter (not . null) [w, lwf ++ l, ldh, lwc, t],
      ppExtras = logLimitWindows
    }
  where
    colorLayout keywords s = fromMaybe s $ do
      pre <- find (`isPrefixOf` s) keywords
      strip <- stripPrefix pre s
      return $ "^fg(yellow2)" ++ pre ++ "^fg()" ++ strip

myNonfocusPPXin :: PP
myNonfocusPPXin = myFocusPPXin {
  ppCurrent = wrap "^bg(lightbg)[  ^fg(blue2)" "  ]^bg()",
  ppSep = " ^fg(blue2):^fg() "
  }

multiPrepare :: String -> Bool -> X PP
multiPrepare output focused = do
  L.updateCurrentState
  showWindows <- ppShowWindows
  wsName <- getWorkspaceNames'
  let pp = if focused then myFocusPPXin else myNonfocusPPXin
  return $
    decoratePP
      (\w -> concatMap ($ w) [colorize, showWindows, maybe "" (":"++) . wsName])
      (pp {ppOutput = statusbarOutput output . fixXinerama pp})
  where
    colorize = wrap "" "^fg()"

    fixXinerama :: PP -> String -> String
    fixXinerama pp s = removeIndices 0 s . tail . init . findIndices (\c -> c == '[' || c == ']') . takeTo (ppSep pp) $ s

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

    statusbarOutput :: String -> String -> IO ()
    statusbarOutput output str =
      catch (do exi <- doesFileExist statusbarFifo
                when exi $ writeFile statusbarFifo ("xmonad_" ++ output ++ " " ++ str ++ "\n"))
            (\e ->
                trace ("Couldn't write to statusbar: " ++ show (e :: IOException)))

-- advertise fullscreen support (which isn't done by the
-- ewmh/fullscreen package for some reason)
-- https://github.com/xmonad/xmonad-contrib/issues/288
fullscreenStartupHook :: X ()
fullscreenStartupHook = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    f <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ do
        sup <- join . maybeToList <$> getWindowProperty32 dpy a r
        when (fromIntegral f `notElem` sup) $
            changeProperty32 dpy r a c propModeAppend [fromIntegral f]

myConfig = def {
  modMask = myModMask,
  borderWidth = 0,
  focusedBorderColor = "#dddddd",
  normalBorderColor = "#555555",
  keys = myKeys,
  workspaces = myWorkspaces,
  manageHook = centerFloatMH <+> toScratchMH <+> manageHook def,
  handleEventHook = dynamicPropertyChange "WM_CLASS" toScratchMH <+> handleEventHook def,
  startupHook = startupHook def <+> myStartupHook <+> fullscreenStartupHook,
  logHook = logHook def <+> myUpdatePointer
  }
  where
    centerFloatMH = composeAll [ isDialog <||> appName =? "URxvtFZF" --> doCenterFloat ]
    toScratchMH = composeAll [appName =?? wc --> doShift scratchWS | wc <- scratchWindows]
    -- lowercase =?
    q =?? s = (map toLower <$> q) =? map toLower s
    scratchWindows = ["spotify", "discord", "deluge", "telegram-desktop-bin"]

errorFile :: FilePath
errorFile = "/tmp/xmonad-error"

main :: IO ()
main = do
  -- custom .xsession-error
  catch (do
            fs <- doesFileExist errorFile
            when fs $ removeFile errorFile
            fd <- createFile errorFile (CMode 0o666)
            _ <- dupTo fd stdError
            when (fd /= stdError) $ closeFd fd
            )
    (\e -> trace (show (e :: SomeException)))

  xmonad $ indiPP format $ withUrgencyHook NoUrgencyHook $ ewmh $ docks $ myConfig {
    layoutHook = avoidStruts myLayoutHook,
    handleEventHook = handleEventHook myConfig <+> fullscreenEventHook,
    logHook = logHook myConfig <+> workspaceHistoryHook <+> workspaceNamesClearerLogHook
    }
  where
    format = multiPP multiPrepare
