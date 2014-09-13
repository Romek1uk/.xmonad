-- Modules
import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import XMonad.Actions.FloatKeys
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

--------------------------------------------------------------------------------------------
-- Workspaces 						 				  --
--------------------------------------------------------------------------------------------
myWorkspaces = ["1:main","2:dev","3:social","4","5","6","7","8:media","9:mail"]


--------------------------------------------------------------------------------------------
-- Keybindings                                                                            --
--------------------------------------------------------------------------------------------
moveAmount = 15

myKeys =
    [((0, xK_Print), spawn "scrot ~/screenshots/%d-%m-%Y-%T-screenshot.png")
    ,((mod4Mask, xK_w), spawn "google-chrome-stable &")
    ,((mod4Mask, xK_s), spawn "dmenu_run -nb black -fn xft:inconsolata-g:size=11:bold:antialias=true &")
    ,((mod4Mask, xK_b), sendMessage ToggleStruts)
    ,((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20")
    ,((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20")
    ,((mod4Mask, xK_l), spawn "i3lock-wrapper")

    -- Moving dialog windows with start+shift+arrows
    ,((mod4Mask .|. shiftMask, xK_Up), withFocused (keysMoveWindow (0, -moveAmount)))
    ,((mod4Mask .|. shiftMask, xK_Down), withFocused (keysMoveWindow (0, moveAmount)))
    ,((mod4Mask .|. shiftMask, xK_Left), withFocused (keysMoveWindow (-moveAmount, 0)))
    ,((mod4Mask .|. shiftMask, xK_Right), withFocused (keysMoveWindow (moveAmount, 0)))
    ]

--------------------------------------------------------------------------------------------
-- Hooks                                                                                  --
--------------------------------------------------------------------------------------------
myManageHook = composeAll
  [className =? "Chrome" --> doShift "3:web"
  ,className =? "Chromiumi Browser" --> doShift "3:web"
  ,className =? "Google-chrome" --> doShift "3:web"
  ,className =? "feh" --> doFloat
  ,className =? "Thunderbird" --> doShift "9:mail"
  ,className =? "Vlc" --> doShift "8:media"
  ,isFullscreen --> doFullFloat
  ,isDialog --> doFloat
  ]


--------------------------------------------------------------------------------------------
-- MAIN                                                                                   --
--------------------------------------------------------------------------------------------
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/romek/.xmonad/xmobarrc"
  spawn "thunderbird"
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , terminal = "urxvt"
    , borderWidth = 3 
    , focusFollowsMouse = False
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "blue" "" . shorten 100
	  , ppHiddenNoWindows = xmobarColor "grey" ""
      , ppLayout = const ""
      } 
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , manageHook = manageDocks <+> myManageHook  <+> manageHook defaultConfig
    } `additionalKeys` myKeys
