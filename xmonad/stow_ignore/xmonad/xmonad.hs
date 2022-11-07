import System.Posix.Types (CMode(..))
import System.Posix.IO (dupTo,closeFd,createFile,stdError,openFd,fdWrite,OpenMode(WriteOnly),defaultFileFlags,OpenFileFlags(nonBlock))
import Control.Exception (catch,bracket,SomeException,IOException)
import System.Directory (doesFileExist,removeFile,executable,getPermissions,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit (exitSuccess)
import Control.Monad (when,void)
import Data.List (find,stripPrefix,isPrefixOf,findIndices)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

import Graphics.X11.ExtraTypes.XF86

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.Warp hiding (banish)
import XMonad.Actions.PhysicalScreens (viewScreen,onNextNeighbour,onPrevNeighbour,sendToScreen)
import XMonad.Actions.SwapWorkspaces (swapTo,swapWithCurrent)
import XMonad.Actions.EasyMotion (selectWindow)

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Types (Direction1D(Prev,Next))

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt (XPConfig,font)

import XMonad.Hooks.DynamicLog (PP(..),wrap,shorten)
import XMonad.Hooks.ManageDocks (ToggleStruts(..),avoidStruts,docks)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen,setEwmhActivateHook,ewmh)
import XMonad.Hooks.ManageHelpers (doCenterFloat,isDialog)
import XMonad.Hooks.UrgencyHook (withUrgencyHook,NoUrgencyHook(..),doAskUrgent)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook,workspaceHistory)
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)

import XMonad.Layout.MultiToggle (mkToggle,single,Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR))
import XMonad.Layout.Renamed (renamed,Rename(CutWordsLeft))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Spacing (spacingRaw,Border(Border))
import XMonad.Layout.SimplestFloat (simplestFloat)

import Erik.MyStuff
import Erik.IndiPP
import qualified Erik.MyLimitWindows as L

myXPConfig :: XPConfig
myXPConfig = def {
  font = "xft:DejaVu Sans:pixelsize=12"
  }

myModMask = mod4Mask
altMask = mod1Mask

statusbarFifo = "/tmp/statusbar_fifo"

scratchWS = "V"

myWorkspaces = map show [1..(9 :: Int)] ++ [scratchWS]

myBaseLayouts = onWorkspace scratchWS (Grid (16/10))
                                      (Tall 1 (3/100) (1/2) ||| simplestFloat)

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
    -- other
    ((modm, xK_w), sendMessage $ Toggle MIRROR),
    ((modm, xK_Tab), selectWindow def >>= flip whenJust (windows . W.focusWindow)),
    ((modm .|. shiftMask, xK_e), spawn "rofi_iconfont_paste emoji \"$HOME/.emoji\""),

    -- limitWindows
    ((modm, xK_y), L.decreaseLimit),
    ((modm, xK_e), L.increaseLimit),
    ((modm, xK_c), L.toggleLimit),
    ((modm, xK_f), L.toggleFull),

    -- empty workspaces
    ((modm, xK_o), windowsLowestEmpty W.view $ XMonad.workspaces conf),
    ((modm .|. shiftMask, xK_o), windowsLowestEmpty shiftView $ XMonad.workspaces conf),
    ((modm .|. controlMask, xK_o), windowsLowestEmpty W.shift $ XMonad.workspaces conf),

    -- executers
    ((modm, xK_x), spawn "fzf_run"),
    ((modm, xK_r), spawn "rofi_script_selector"),
    ((modm .|. shiftMask, xK_r), spawn "open_downloaded_pdf"),

    -- screens
    ((modm .|. controlMask, xK_j), onNextNeighbour def W.view),
    ((modm .|. controlMask, xK_k), onPrevNeighbour def W.view),
    ((modm .|. controlMask .|. shiftMask, xK_j), onNextNeighbour def swapWith),
    ((modm .|. controlMask .|. shiftMask, xK_k), onPrevNeighbour def swapWith),
    ((modm .|. controlMask, xK_b), screenWorkspace 0 >>= flip whenJust (windows . W.view)),

    -- printscreen
    ((0, xK_Print), spawn "maim_clipboard -su"),
    ((controlMask, xK_Print), spawn "maim-notify -su"),
    ((shiftMask, xK_Print), spawn "maim-notify -u"),

    -- flash
    ((modm, xK_z), spawn "flasher"),

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

    -- theme
    ((modm, xK_minus), spawn "theme_select_safe"),
    ((modm .|. shiftMask, xK_minus), spawn "theme_select multi-random"),

    -- brightness
    ((modm, xK_Left), spawn "i3_brightness -steps 1 -dec 1"),
    ((modm, xK_Right), spawn "i3_brightness -steps 1 -inc 1"),
    ((0, xF86XK_MonBrightnessUp), spawn "i3_brightness -steps 1 -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "i3_brightness -steps 1 -dec 10"),

    -- media buttons
    ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
    ((0, xF86XK_AudioNext), spawn "playerctl next"),
    ((0, xF86XK_AudioPrev), spawn "playerctl previous"),

    -- launch a terminal
    ((modm, xK_Return), spawn "st-tmux"),
    ((modm .|. altMask, xK_Return), spawn "st"),
    ((modm .|. controlMask, xK_Return), spawn "st-tmux-ranger"),
    ((modm .|. shiftMask, xK_Return), spawn "emacsclient -nc"),

    -- close focused window
    ((modm, xK_q), kill),

     -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),
    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),

    -- Resize viewed windows to the correct size
    ((modm, xK_n), refresh),

    -- Pointer stuff
    ((modm, xK_p), banish),
    ((modm .|. shiftMask, xK_p), warpToWindow 0.5 0.5),
    ((modm .|. controlMask, xK_p), myUpdatePointerToggle),

    -- Move focus to the previous window
    ((modm, xK_k), windows W.focusUp),
    ((modm, xK_j), windows W.focusDown),
    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows W.swapDown),
    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows W.swapUp),

    -- Move focus to the master window
    ((modm, xK_b), windows W.focusMaster),
    -- Swap the focused window and the master window
    ((modm .|. shiftMask, xK_b), windows W.swapMaster),

    -- moves workspaces up or down
    ((modm, xK_period), swapTo Next),
    ((modm, xK_comma), swapTo Prev),
    ((modm .|. shiftMask, xK_comma), windows $ swapWithCurrent $ head myWorkspaces),

    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),
    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),

    -- Increment the number of windows in the master area
    ((modm .|. shiftMask, xK_h), sendMessage (IncMasterN 1)),
    -- Deincrement the number of windows in the master area
    ((modm .|. shiftMask, xK_l), sendMessage (IncMasterN (-1))),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),
    ((modm .|. shiftMask, xK_t), withFocused $ centerFloat 700 500),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    ((modm, xK_u), sendMessage ToggleStruts),
    ((modm .|. shiftMask, xK_u), toggleMapStruts),

    -- notifications
    ((modm, xK_BackSpace), spawn "dunstctl close-all"),
    ((modm .|. controlMask, xK_BackSpace), spawn "dunstctl history-pop"),

    -- Quit xmonad
    ((modm .|. shiftMask, xK_0), confirmPrompt myXPConfig "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt myXPConfig "power off?" $ spawn "poweroff")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-control-[1..9], Move client to workspace N
    -- mod-shift-[1..9], Move client and swith to workspace N
    --
    [((m .|. modm, k), windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_v])
        , (f, m) <- [(W.greedyView, 0),
                     (shiftView, shiftMask),
                     (W.shift, controlMask)
                    ]]

    ++

    -- NOTE: overwrite greedyView from above
    [((modm, xK_v), scratchVisit)]

    ++

    --
    -- mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,s,d}, Move client to screen 1, 2, or 3 and view
    -- mod-control-{a,s,d}, Move window
    -- mod-shift-control-{a,s,d}, move screen
    --
    [((m .|. modm, key), f sc)
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(viewScreen def, 0),
                     (\i -> sendToScreen def i >> viewScreen def i, shiftMask),
                     (sendToScreen def, controlMask)
                    ]]

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
  let pp = if focused then myFocusPPXin else myNonfocusPPXin
  return $
    decoratePP
      (\w -> concatMap ($ w) [colorize, showWindows])
      (pp {ppOutput = statusbarOutput . fixXinerama pp})
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

    pipe_buf = 4096 -- maximum size to be atomic
    toUtfStr :: String -> String
    toUtfStr str = take (pipe_buf - 1) (UTF8.decodeString $ concat ["xmonad_", output, " ", str])
                   ++ "\n"

    statusbarOutput :: String -> IO ()
    statusbarOutput str =
      catch (bracket (openFd statusbarFifo WriteOnly Nothing defaultFileFlags{nonBlock = True})
                     closeFd
                     (\fd -> void $ fdWrite fd $ toUtfStr str))
            (\e ->
                trace ("Couldn't write to statusbar: " ++ show (e :: IOException)))

myConfig = def {
  modMask = myModMask,
  borderWidth = 0,
  focusedBorderColor = "#dddddd",
  normalBorderColor = "#555555",
  keys = myKeys,
  workspaces = myWorkspaces,
  manageHook = centerFloatMH <+> toScratchMH <+> manageHook def,
  handleEventHook = dynamicPropertyChange "WM_CLASS" toScratchMH <+> handleEventHook def,
  startupHook = startupHook def <+> myStartupHook,
  layoutHook = avoidStruts myLayoutHook,
  logHook = logHook def <+> myUpdatePointer <+> workspaceHistoryHook
  }
  where
    centerFloatMH = composeAll [ appName =? "URxvtFZF" --> doCenterFloat, isDialog --> doFloat ]
    toScratchMH = composeAll [appName =?? wc --> doShift scratchWS | wc <- scratchWindows]
    -- lowercase =?
    q =?? s = (map toLower <$> q) =? map toLower s
    scratchWindows = ["spotify", "discord", "deluge", "telegram-desktop"]
    --TODO: add if it is a problem with trayer on the bottom. These lines should be correct.
    -- import XMonad.Util.Hacks (trayAbovePanelEventHook)
    -- trayer = trayAbovePanelEventHook (className =? "trayer") (className =? "dzen")

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

  xmonad . indiPP format . withUrgencyHook NoUrgencyHook . ewmhstuff . docks $ myConfig
  where
    format = multiPP multiPrepare
    ewmhstuff = ewmhFullscreen . setEwmhActivateHook doAskUrgent . ewmh
