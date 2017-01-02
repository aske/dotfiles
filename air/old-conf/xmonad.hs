import           XMonad                               hiding ((|||))
import           XMonad.Config.Gnome
import           XMonad.Core

import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Util.EZConfig                 (additionalKeysP)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Run                      (spawnPipe)

import           XMonad.Prompt
import           XMonad.Prompt.Shell

import           XMonad.Actions.WindowGo              (runOrRaise)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.WindowNavigation

import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.Search                as S
import           XMonad.Actions.SpawnOn

import           XMonad.Layout.LayoutCombinators

import XMonad.Layout.ThreeColumns

import           System.IO

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
  spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --tint 0x0000000 --height 8 &"
  spawn "urxvtd"
  xmonad $ gnomeConfig
    { modMask = mod4Mask,
      borderWidth = 2,
      terminal = myTerminal,
      focusFollowsMouse = False,
      normalBorderColor = myInactiveBorderColor,
      focusedBorderColor = myActiveBorderColor,
      layoutHook = avoidStruts $ smartBorders $ myLayout,
      workspaces = myWorkspaces,
      manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig,
      logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = xmobarColor "white" "" . shorten 90,
                  ppLayout = const ""
                }
    } `additionalKeysP` addKeys

myTerminal = "urxvtc"

addKeys = [ ("S-M-l", spawn "gnome-screensaver-command -l"),
            ("M-p", shellPrompt myXPConfig),
            ("M-q", spawn "killall trayer; xmonad --recompile; xmonad --restart"),
            ("S-M-q", spawn "gnome-session-quit --power-off"),
            -- ("M-x f", runOrRaise "firefox" (className =? "Firefox")),
            ("M-x f", spawn "firefox"),
            ("M-x e", spawn "emacs"),
            ("M-x g c", spawn "gnome-control-center"),
            ("M-x g s", spawn "gnome-system-monitor"),
            ("M-x t", spawn myTerminal),
            ("M-x n", spawn "nautilus"),
            ("M-x m", spawn "urxvt -e ranger"),
            ("M-x d", spawn "deadbeef"),
            ("M-x p", runOrRaise "pidgin" (className =? "Pidgin")),
            ("M-x s", runOrRaise "skype" (className =? "Skype")),
            ("M-x l", runOrRaise "liferea" (className =? "Liferea")),
            ("M-e", toggleWS),
            ("M-<F1>", sendMessage $ JumpToLayout "Tall"),
            ("M-<F2>", sendMessage $ JumpToLayout "Mirror Tall"),
            ("M-<F3>", sendMessage $ JumpToLayout "Full"),
            ("M-f", sendMessage $ JumpToLayout "Full")
          ]
          ++
          [("M-C-c " ++ k, S.selectSearch f) | (k,f) <- searchList ]
          ++
          [("M-c " ++ k, S.promptSearch bigXPConfig f) | (k,f) <- searchList ]

ruwikipedia, wolfram, hoogle :: S.SearchEngine
ruwikipedia = S.searchEngine "ruwikipedia" "http://ru.wikipedia.org/wiki/Special:Search?go=Go&search="
wolfram = S.searchEngine "wolfram" "http://www.wolframalpha.com/input/?i="
hoogle = S.searchEngine "hoogle" "http://www.haskell.org/hoogle/?hoogle="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.google),
               ("w", S.wikipedia),
               ("r", ruwikipedia),
               ("y", S.youtube),
               ("f", wolfram),
               ("h", hoogle)
             ]

myFont = "xft:Monaco:size=12:normal:antialias=true:hinting=true:hintstyle=hintslight"
myBgColor = "black"
myFgColor = "grey"
--myActiveBorderColor = "blue3"
myActiveBorderColor = "yellow"
myInactiveBorderColor = "gray20"

myWorkspaces = ["1:term",
                "2:emacs",
                "3:web",
                "4:misc",
                "5:apps",
                "6:",
                "7:media",
                "8:im",
                "9:",
                "0:"
               ]

myXPConfig = defaultXPConfig { font = myFont,
                               bgColor = myBgColor,
                               fgColor = myFgColor,
                               borderColor = myActiveBorderColor,
                               position = Bottom
                             }

bigXPConfig = myXPConfig
              { height = 30 }

myManageHook = composeAll
  [ className =? "Pidgin" --> doShift "8:im",
    className =? "Skype" --> doShift "8:im",
    className =? "Deadbeef" --> doShift "7:media",
    className =? "Gimp"  --> doFloat,
    -- className =? "Evince" --> doShift "5:apps",
    className =? "Liferea" --> doShift "6:",
    isFullscreen --> doFullFloat
  ]

defaultLayouts = tiled ||| Mirror tiled ||| Full
  where
      -- default tiling algorithm partitions the screen into two panes
      tiled = Tall nmaster delta ratio
      -- The default number of windows in the master pane
      nmaster = 1
      -- Default proportion of screen occupied by master pane
      ratio = 1/2
      -- Percent of screen to increment by when resizing panes
      delta = 5/100

additionalLayouts = ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2)

myLayout = defaultLayouts ||| additionalLayouts
