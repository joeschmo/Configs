import XMonad
import XMonad.ManageHook
import XMonad.Hooks.FloatNext
import XMonad.Util.Run
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Layout.Minimize
import XMonad.Actions.TagWindows
import XMonad.Actions.RotSlaves
import System.Exit

import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "urxvt"

myBorderWidth = 1

myModMask = mod4Mask

myWorkspaces = ["main", "web", "media", "sysinfo"] ++ map show [5..10]

myNormalBorderColor = "#333333"
myFocusedBorderColor = "#3465a4"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_p), shellPrompt defaultXPConfig)
    , ((modm .|. shiftMask, xK_p), spawn "gmrun")
    , ((modm .|. shiftMask, xK_s), sshPrompt defaultXPConfig)
    , ((modm .|. shiftMask, xK_c), kill)
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_n), refresh)
    , ((modm .|. shiftMask, xK_f), toggleFloatNext)
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab), rotSlavesUp)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm .|. shiftMask, xK_m), withFocused minimizeWindow)
    , ((modm .|. controlMask, xK_m), sendMessage RestoreNextMinimizedWin)
    , ((modm, xK_g), tagPrompt defaultXPConfig (\s -> withFocused (addTag s)))
    , ((modm .|. controlMask, xK_g), tagDelPrompt defaultXPConfig)
    , ((modm .|. shiftMask, xK_t), withFocused (addTag "a"))
    , ((modm .|. controlMask, xK_t), withFocused (delTag "a"))
    , ((modm .|. shiftMask, xK_d), withTaggedGlobalP "a" shiftHere)
    , ((modm .|. shiftMask, xK_b), tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s shiftHere))
    , ((modm, xK_b), sendMessage ToggleStruts)
    , ((modm, xK_t), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((modm, xK_q), restart "xmonad" True)
    , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command --lock")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i,k) <- zip (XMonad.workspaces conf) [xK_1..xK_9]
        , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myManageHook = composeAll $
    [ className =? "Gimp" --> doFloat
    , className =? "Chromium" --> doF(W.shift "web")
    , className =? "Firefox" --> doF(W.shift "web")
    , className =? "mplayer" --> doF(W.shift "media")
    , className =? "vlc" --> doF(W.shift "media")
    , className =? "htop" --> doF(W.shift "sysinfo")
    , className =? "wicd-curses" --> doF(W.shift "sysinfo")
    , className =? "multitail" --> doF(W.shift "sysinfo")
    ]

myLayout = minimize $ avoidStruts(tiled ||| Mirror tiled ||| Full) ||| Full
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myFocusFollowsMouse = True

myStartupHook = return ()

main = do
    h <- spawnPipe "xmobar /home/joseph/.xmobarrc"
    xmonad $ defaultConfig 
        { terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , borderWidth = myBorderWidth
        , modMask = myModMask
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , layoutHook = smartBorders $ myLayout
        , manageHook = floatNextHook <+> myManageHook
        , logHook = dynamicLogWithPP $ defaultPP { 
            ppOutput = hPutStrLn h,
            ppCurrent = xmobarColor "white" "#3465a4" . wrap " " " ",
            ppTitle = xmobarColor "green" "" . wrap " " " " }
        , startupHook = myStartupHook
        }
