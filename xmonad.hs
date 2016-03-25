--------------------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------------------
import XMonad
import XMonad.Prompt
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Accordion
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Actions.FloatKeys
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

--------------------------------------------------------------------------------------------
-- Colours                                                                                -
--------------------------------------------------------------------------------------------
-- Borders
disabledBorderColor = "#444444" -- "#284F51"
enabledBorderColor  = "#FFB347"

-- Xmobar
currentForegroundColor = "black"
currentBackgroundColor = enabledBorderColor 
hiddenForegroundColor  = enabledBorderColor-- "#6666FF"
hiddenBackgroundColor  = "black"
emptyForegroundColor   = "#777777"
emptyBackgroundColor   = ""
titleForegroundColor   = "white" -- "#26C6C6"
titleBackgroundColor   = ""

-------------------------------------------------------------------------------------------- 
---- Workspaces 						 				                                                      --
-------------------------------------------------------------------------------------------- 
myWorkspaces = map (wrap " " " ")[ "1:main","2","3:dev","4:diss","5","6","7","8:soc","9:mail"]
mailWorkspace = " 9:mail "

--------------------------------------------------------------------------------------------
-- Layouts                                                                                --
--------------------------------------------------------------------------------------------
spacingAmount = 15
spacingAmountAccordion = 5

myLayouts = tiled ||| full ||| threecol 
  where
    tiled = smartSpacing spacingAmount $ smartBorders $ Tall nmaster delta ratio
    full = smartBorders $ Full
    threecol = smartSpacing spacingAmount $ smartBorders $ ThreeCol nmaster delta ratio
    -- grid = smartSpacing spacingAmount $ smartBorders $ Grid
    nmaster = 1 -- Number of windows in master pane
    ratio = 1/2 -- Proportion of screen occupied by master pane
    delta = 3/100 -- Percent of screen to increment by when resizing panes   

--------------------------------------------------------------------------------------------
-- Keybindings                                                                            --
--------------------------------------------------------------------------------------------
shiftAmount = 15 -- How much a window moves, in pxels

myKeys =
    [((0, xK_Print), spawn "scrot ~/screenshots/%d-%m-%Y-%T-screenshot.png")
    ,((mod4Mask, xK_w), spawn "google-chrome-stable")
    ,((mod4Mask, xK_p), spawn "dmenu_run -nb black")
    ,((mod4Mask, xK_b), sendMessage ToggleStruts)
    ,((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20")
    ,((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20")
    ,((mod4Mask .|. controlMask, xK_l), spawn "slimlock")
    ,((0, xF86XK_AudioLowerVolume), spawn "amixer -c 0 set Master 5-")
    ,((0, xF86XK_AudioRaiseVolume), spawn "amixer -c 0 set Master 5+")
    ,((0, xF86XK_AudioMute), spawn "amixer -c 0 set Master toggle")

    -- Moving dialog windows with start+shift+arrows
    ,((mod4Mask .|. controlMask, xK_Up), withFocused (keysMoveWindow (0, -shiftAmount)))
    ,((mod4Mask .|. controlMask, xK_Down), withFocused (keysMoveWindow (0, shiftAmount)))
    ,((mod4Mask .|. controlMask, xK_Left), withFocused (keysMoveWindow (-shiftAmount, 0)))
    ,((mod4Mask .|. controlMask, xK_Right), withFocused (keysMoveWindow (shiftAmount, 0)))
    
    -- Moving workspaces with left/right arrow keys
    ,((mod4Mask, xK_Right), nextWS)
    ,((mod4Mask, xK_Left), prevWS)
    ,((mod4Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    ,((mod4Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    ,((0, xK_Insert), pasteSelection)
    ]

--------------------------------------------------------------------------------------------
-- Hooks                                                                                  --
--------------------------------------------------------------------------------------------
myManageHook = composeAll
  [className =? "feh" --> doFloat
  ,className =? "Thunderbird" --> doShift mailWorkspace
  -- ,className =? "Vlc" --> doShift mediaWorkspace
  ,isFullscreen --> doFullFloat
  ,isDialog --> doFloat
  ]

--------------------------------------------------------------------------------------------
-- MAIN                                                                                   --
--------------------------------------------------------------------------------------------
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/romek/.xmonad/xmobarrc"
  spawn "thunderbird"
  spawn "dropbox start"
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , terminal = "urxvt"
    , borderWidth = 4 
    , normalBorderColor = disabledBorderColor
    , focusedBorderColor = enabledBorderColor
    , focusFollowsMouse = False
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor titleForegroundColor titleBackgroundColor . shorten 110
	    , ppHiddenNoWindows = xmobarColor emptyForegroundColor emptyBackgroundColor
      , ppLayout = const ""
      , ppHidden = xmobarColor hiddenForegroundColor hiddenBackgroundColor
      , ppCurrent = xmobarColor currentForegroundColor currentBackgroundColor
      } 
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts $ myLayouts
    , manageHook = manageDocks <+> myManageHook  <+> manageHook defaultConfig
    } `additionalKeys` myKeys
