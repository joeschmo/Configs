import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh

import qualified XMonad.StackSet as W

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myBar = "xmobar"

-- Look at the status bar. Now you know.
myPP = xmobarPP { ppCurrent = xmobarColor "green" "" . wrap "[" "]" }

-- mod + B to hide status bar
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myManageHook = composeAll
    [ className =? "Chromium" --> doF(W.shift "2:web")
    , className =? "Firefox" --> doF(W.shift "2:web")
    -- VLC, why u no listen?!
    , className =? "VLC media player" --> doF(W.shift "3:media")
    , className =? "MPlayer" --> doF(W.shift "3:media")
    ]

myConfig = defaultConfig { borderWidth = 1
                         -- for opening things in their appropriate workspaces
                         , manageHook = myManageHook
                         , terminal = "urxvt"
                         -- grey unfocused border
                         , normalBorderColor = "#333333"
                         -- darkblue focused border
                         , focusedBorderColor = "#3465a4"
                         , workspaces = ["1:term", "2:web", "3:media"] ++ map show [4..10]
                         -- windows key is mod key
                         , modMask = mod4Mask 
                         -- no border when full screen
                         , layoutHook = smartBorders $ layoutHook defaultConfig
                         } `additionalKeys`
                         [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command --lock")
                         , ((mod4Mask, xK_p), shellPrompt defaultXPConfig)
                         , ((mod4Mask .|. shiftMask, xK_s), sshPrompt defaultXPConfig)
                         ]
