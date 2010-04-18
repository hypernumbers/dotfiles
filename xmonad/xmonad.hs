import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoBorders

import Data.Monoid
import System.Exit
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- The preferred terminal program
myTerminal = "gnome-terminal -e screen"
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = True
 
-- Width of the window border in pixels.
myBorderWidth   = 0
 
-- modMask lets you specify which modkey you want to use.
myModMask       = mod4Mask
  
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
  
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [  
      -- Print Screen (ubuntu)
      ((0, xK_Print ), spawn "gnome-screenshot --interactive"),

      ((modm, xK_c), kill),

      -- Toggle between layouts
      ((modm, xK_space ), sendMessage NextLayout),

      -- Reset layout to default
      ((modm .|. shiftMask, xK_space ), 
       setLayout $ XMonad.layoutHook conf),
 
      -- Move focus to the next window
      ((modm, xK_Tab),   windows W.focusDown),
      ((modm, xK_Right), windows W.focusDown),
                                                            
      -- Move focus to the previous window
      ((modm, xK_Left), windows W.focusUp),

      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_Left),  windows W.swapUp),
      ((modm .|. shiftMask, xK_Right), windows W.swapDown),

      -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster),
 
      -- Set current window to master
      ((modm, xK_Return), windows W.swapMaster),
  
      -- Expand / Shrink the master area (horizontal)
      ((modm, xK_s), sendMessage Shrink),
      ((modm, xK_d), sendMessage Expand),

      -- Expand / Shrink the mirror area
      ((modm, xK_z), sendMessage MirrorShrink),
      ((modm, xK_a), sendMessage MirrorExpand),
 
      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink),
   
      -- Restart xmonad
      ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
-- which denotes layout choice.
--
myLayout = ResizableTall 1 (1/100) (0.42) [] ||| Full

-- Ignore or float by default some windows
-- 
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ -- Make fullscreen(flash) windows work
      isFullscreen --> doFullFloat,
      className =? "Gimp"           --> doFloat, 
      className =? "Skype"          --> doFloat,
      className =? "Synaptic"       --> doFloat, 
      -- Firefox's Download Manager
      resource  =? "Download"       --> doFloat, 
      resource  =? "Do"             --> doIgnore,
      resource  =? "gnome-panel"    --> doIgnore,
      resource  =? "desktop_window" --> doIgnore,
      resource  =? "kdesktop"       --> doIgnore ]
 
 
-- Fade the inactive windows
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.7
 
main = xmonad defaults
 
defaults = defaultConfig {

      manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,

      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = onWorkspace "4" simplestFloat $ avoidStruts ( myLayout ),
        logHook            = myLogHook
    }
