import Graphics.X11.ExtraTypes.XF86

import XMonad

import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.Place

import XMonad.ManageHook

import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns

import XMonad.Prompt

import System.IO

import qualified XMonad.StackSet as W

myModMask = mod4Mask
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myTerminal = "urxvt"
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#3465a4"

myKeys =
  [ ("M-S-l",           spawn "dm-tool lock")
  , ("M-p",             spawn "rofi -show run")
  , ("M-S-<Space>",     sendMessage ToggleLayout)
  , ("M-S-<Backspace>", removeWorkspace)
  , ("M-C-<Return>",    addWorkspacePrompt def)
  , ("M-S-v",           selectWorkspace def)
  , ("M-m",             withWorkspace def (windows . W.shift))
  , ("M-S-m",           withWorkspace def (windows . copy))
  , ("M-S-r",           renameWorkspace def)
  , ("M-h",             prevWS)
  , ("M-l",             nextWS)
  , ("M-d",             sendMessage Expand)
  , ("M-a",             sendMessage Shrink)
  , ("M-f",             withFocused $ addTag "misc")
  , ("M-C-f",           withFocused $ delTag "misc")
  , ("M-S-f",           toggleFloatNext >> spawn myTerminal)
  , ("M-g",             withTaggedGlobalP "misc" shiftHere)
  ]

bspKeys =
  [ ("M-S-C-M1-l", sendMessage $ ExpandTowards R)
  , ("M-S-C-M1-h", sendMessage $ ExpandTowards L)
  , ("M-S-C-M1-j", sendMessage $ ExpandTowards D)
  , ("M-S-C-M1-k", sendMessage $ ExpandTowards U)
  , ("M-C-M1-l",   sendMessage $ ShrinkFrom R)
  , ("M-C-M1-h",   sendMessage $ ShrinkFrom L)
  , ("M-C-M1-j",   sendMessage $ ShrinkFrom D)
  , ("M-C-M1-k",   sendMessage $ ShrinkFrom U)
  , ("M-r",        sendMessage Rotate)
  , ("M-s",        sendMessage Swap)
  , ("M-n",        sendMessage FocusParent)
  , ("M-C-n",      sendMessage SelectNode)
  , ("M-S-n",      sendMessage MoveNode)
  , ("M-a",        sendMessage Balance)
  , ("M-S-a",      sendMessage Equalize)
  ]

myLayout =
  smartSpacing 6 $
  smartBorders $
  toggleLayouts Full $
    avoidStruts $
          emptyBSP
      ||| ThreeCol 1 (3/100) (1/3)
      ||| Mirror tiled
      ||| spiral (6/7)
      ||| Accordion
      ||| Full
      ||| tiled
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 1/50

myXmobarLoghook handle = dynamicLogWithPP $ def {
  ppOutput = hPutStrLn handle,
  ppCurrent = xmobarColor "white" "#3465a4" . wrap " " " ",
  ppVisible = xmobarColor "white" "#34a465" . wrap " " " ",
  ppTitle = xmobarColor "#00aaaa" "" . wrap " " " "
}

programSpecificManageHooks = composeAll
  [ className =? "feh" --> doFloat
  ]

myManageHook = placeHook (fixed (0.5, 0.5))
  <+> floatNextHook
  <+> programSpecificManageHooks
  <+> manageHook def
  <+> manageDocks

main = do
  xmproc <- spawnPipe "pkill xmobar; xmobar ~/.xmobarrc"
  spawn "pkill dunst; dunst -config ~/.dunstrc"
  xmonad $ ewmh def
    { modMask = mod4Mask
    , terminal = myTerminal
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , layoutHook = myLayout
    , logHook = myXmobarLoghook xmproc
    , manageHook = myManageHook
    , handleEventHook = docksEventHook
                    <+> handleEventHook def
                    <+> fullscreenEventHook
    } `additionalKeysP` (myKeys ++ bspKeys)
      `additionalKeys`
      [ ((0, xF86XK_AudioLowerVolume ), spawn "amixer set Master 2-")
      , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 2+")
      ]
