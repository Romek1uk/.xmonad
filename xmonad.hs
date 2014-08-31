-- Modules
import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Define amount and names of workspaces
myWorkspaces = ["main","dev","web","mail","5","6", "7", "8", "9"]

--------------------------------------------------------------------------------------------
-- MAIN                                                                                   --
--------------------------------------------------------------------------------------------
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/romek/.xmonad/xmobarrc"
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , terminal = "xterm"
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "blue" "" . shorten 50
	  , ppHiddenNoWindows = xmobarColor "grey" ""
      , ppLayout = const ""
      } 
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , manageHook = manageDocks  <+> manageHook defaultConfig
    } `additionalKeys`
    [((0, xK_Print), spawn "scrot ~/screenshots/%d-%m-%Y-%T-screenshot.png")
    ]
