import System.Posix.Types (CMode(..))
import System.Posix.IO (dupTo,closeFd,createFile,stdError,openFd,fdWrite,OpenMode(WriteOnly),defaultFileFlags,OpenFileFlags(nonBlock))
import Control.Exception (catch,bracket,SomeException,IOException)
import System.Directory (doesFileExist,removeFile,executable,getPermissions,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit (exitSuccess)
import Control.Monad (when,void)
import Data.List (find,stripPrefix,isPrefixOf,findIndices)
import Data.Maybe (fromMaybe)

import Graphics.X11.ExtraTypes.XF86

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.PhysicalScreens (viewScreen,sendToScreen)
import XMonad.Actions.SwapWorkspaces (swapTo,swapWithCurrent)

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
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

import XMonad.Layout.MultiToggle (mkToggle,single,Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL))
import XMonad.Layout.Renamed (renamed,Rename(CutWordsLeft))
import XMonad.Layout.Spacing (spacingRaw,Border(Border))
import XMonad.Layout.SimplestFloat (simplestFloat)

import Erik.MyStuff
import Erik.IndiPP

myXPConfig :: XPConfig
myXPConfig = def {
  font = "xft:DejaVu Sans:pixelsize=12"
  }

runXmonadStartupOnce :: X ()
runXmonadStartupOnce = do
  home <- io getHomeDirectory
  let startupFile = home </> ".xmonad_startup"
  itExists <- io $ doesFileExist startupFile
  when itExists $ do
    perms <- io $ getPermissions startupFile
    when (executable perms) $
      spawnOnce startupFile

superMask = mod4Mask
shift = (.|.) shiftMask

myKeys conf@XConfig {XMonad.modMask = modm, XMonad.workspaces = spaces} =
  M.fromList $
  [
    -- empty workspaces
    ((modm, xK_o), windowsLowestEmpty W.view spaces),
    ((shift modm, xK_o), windowsLowestEmpty W.shift spaces),

    -- executers
    ((modm, xK_e), spawn "fzf_run"),
    ((shift modm, xK_e), spawn "rofi_iconfont_paste emoji \"$HOME/.emoji\""),
    ((modm, xK_r), spawn "rofi_script_selector"),
    ((shift modm, xK_r), spawn "open_downloaded_pdf"),

    -- screenshots
    ((0, xK_Print), spawn "maim-notify -su"), -- screenshot selection
    ((modm, xK_p), spawn "maim-notify -su"),
    ((shiftMask, xK_Print), spawn "maim-current-window"), -- screenshot current window
    ((shift modm, xK_p), spawn "maim-current-window"),

    -- toggle prog mode
    ((shift modm, xK_m), spawn "prog_mode_toggle"),
    ((modm, xK_m), spawn "prog_mode_toggle swetoggle"),

    -- display stuff
    ((modm, xK_plus), spawn "display_updater all"),
    ((modm, xK_apostrophe), spawn "xrandr-invert-colors"),
    ((modm, xK_asciicircum), spawn "pkill picom"),

    -- theme
    ((modm, xK_minus), spawn "theme_select_safe"),
    ((shift modm, xK_minus), spawn "theme_select multi-random"),

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
    ((shift modm, xK_Return), spawn "xterm"),

    -- launch a program
    ((shift modm, xK_i), spawn "st-tmux-ranger"),
    ((modm, xK_i), spawn "emacsclient -nc"),

    -- close focused window
    ((modm, xK_q), kill),
    -- Resize viewed windows to the correct size
    ((shift modm, xK_q), refresh),

     -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),
    --  Reset the layouts on the current workspace to default
    ((shift modm, xK_space), setLayout $ XMonad.layoutHook conf),

    -- Move focus to the previous window
    ((modm, xK_k), windows W.focusUp),
    ((modm, xK_j), windows W.focusDown),
    ((shift modm, xK_j), windows W.swapDown),
    ((shift modm, xK_k), windows W.swapUp),

    -- Move focus to the master window
    ((modm, xK_g), windows W.focusMaster),
    ((shift modm, xK_g), windows W.swapMaster),

    -- moves workspaces up or down
    ((modm, xK_period), swapTo Next),
    ((modm, xK_comma), swapTo Prev),
    ((shift modm, xK_comma), windows $ swapWithCurrent $ head spaces),

    -- modify layout
    ((modm, xK_f), sendMessage $ Toggle FULL),
    ((modm, xK_h), sendMessage Shrink),
    ((modm, xK_l), sendMessage Expand),
    ((shift modm, xK_h), sendMessage (IncMasterN 1)),
    ((shift modm, xK_l), sendMessage (IncMasterN (-1))),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),
    ((shift modm, xK_t), withFocused $ centerFloat 700 500),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    ((modm, xK_u), sendMessage ToggleStruts),
    ((shift modm, xK_u), toggleMapStruts),

    -- notifications
    ((modm, xK_n), spawn "dunstctl close-all"),
    ((shift modm, xK_n), spawn "dunstctl history-pop"),

    -- Quit xmonad
    ((shift modm, xK_0), confirmPrompt myXPConfig "logout?" $ io exitSuccess),
    ((modm, xK_0), confirmPrompt myXPConfig "power off?" $ spawn "poweroff")
    ]
    ++

    --
    -- mod-[z..b], Switch to workspace N
    -- mod-shift-[z..b], Move client to workspace N
    --
    [((m .|. modm, k), windows (f i))
        | (i, k) <- zip spaces [xK_c, xK_v, xK_x, xK_b, xK_z]
        , (f, m) <- [(W.greedyView, 0),
                     (W.shift, shiftMask)
                    ]]

    ++

    --
    -- mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,s,d}, Move window
    --
    [((m .|. modm, key), f sc)
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(viewScreen def, 0),
                     (sendToScreen def, shiftMask)
                    ]]

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
      ppLayout = colorLayout ["Full"],
      ppSort = getSortByXineramaPhysicalRule def
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

    statusbarFifo :: FilePath
    statusbarFifo = "/tmp/statusbar_fifo"

    statusbarOutput :: String -> IO ()
    statusbarOutput str =
      catch (bracket (openFd statusbarFifo WriteOnly Nothing defaultFileFlags{nonBlock = True})
                     closeFd
                     (\fd -> void $ fdWrite fd $ toUtfStr str))
            (\e ->
                trace ("Couldn't write to statusbar: " ++ show (e :: IOException)))

myLayoutHook =
  renamed [CutWordsLeft 1] $ -- remove smartspacing text
  spacingRaw True (Border 3 3 3 3) True (Border 3 3 3 3) True $
  mkToggle (single FULL) $
  Tall 1 (3/100) (1/2) ||| simplestFloat

myConfig = def {
  modMask = superMask,
  borderWidth = 0,
  focusedBorderColor = "#dddddd",
  normalBorderColor = "#555555",
  keys = myKeys,
  workspaces = ["C", "V", "X", "B", "Z"],
  manageHook = centerFloatMH <+> manageHook def,
  startupHook = startupHook def <+> runXmonadStartupOnce <+> setDefaultCursor xC_left_ptr,
  layoutHook = avoidStruts myLayoutHook,
  logHook = logHook def <+> myUpdatePointer <+> workspaceHistoryHook
  }
  where
    centerFloatMH = composeAll [ appName =? "URxvtFZF" --> doCenterFloat, isDialog --> doFloat ]

setStderr :: IO ()
setStderr =
  catch
    (do fs <- doesFileExist errorFile
        when fs $ removeFile errorFile
        fd <- createFile errorFile (CMode 0o666)
        _ <- dupTo fd stdError
        when (fd /= stdError) $ closeFd fd)
    (\e -> trace (show (e :: SomeException)))
  where
    errorFile :: FilePath
    errorFile = "/tmp/xmonad-error"


main :: IO ()
main = do
  setStderr
  xmonad . indiPP format . withUrgencyHook NoUrgencyHook . ewmhstuff . docks $ myConfig
  where
    format = multiPP multiPrepare
    ewmhstuff = ewmhFullscreen . setEwmhActivateHook doAskUrgent . ewmh
