--     ___ ___  ___ ___ _______ ______  _______ ______   
--    (   Y   )|   Y   |   _   |   _  \|   _   |   _  \  
--     \  1  / |.      |.  |   |.  |   |.  1   |.  |   \ 
--     /  _  \ |. \_/  |.  |   |.  |   |.  _   |.  |    \
--    /:  |   \|:  |   |:  1   |:  |   |:  |   |:  1    /
--   (::. |:.  |::.|:. |::.. . |::.|   |::.|:. |::.. . / 
--    `--- ---'`--- ---`-------`--- ---`--- ---`------'  
--              agonzal @ github / dotfiles.git 

import XMonad
import Data.Monoid ()
import Data.List (isSuffixOf, sortBy)
import Data.Function (on)
import System.Exit ()
import System.IO
import Data.Maybe (maybeToList)
import Control.Monad ( join, when, forM_ )
import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Codec.Binary.UTF8.String as UTF8

-- XMonad Utils
import XMonad.Util.WorkspaceCompare 
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.Run (safeSpawn, spawnPipe, runInTerm)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)

-- XMonad Graphics 
import Graphics.X11.ExtraTypes.XF86 
    (xF86XK_Mail, xF86XK_WWW, xF86XK_Search, xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)

-- XMonad Hooks 
import XMonad.Hooks.ManageDocks
    ( ToggleStruts(..), avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..)) 
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen, doCenterFloat, isDialog )
import XMonad.Hooks.Minimize


-- FancyBorders (double borders)
import FancyBorders

-- XMonad Actions
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn) 
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Util.NamedWindows (getName, NamedWindow)

-- XMonad Layout Imports 

import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhDesktopsEventHook, ewmhDesktopsLogHook, ewmhDesktopsStartup )
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral ( spiral )
import XMonad.Layout.ThreeColumns 
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.SimpleFloat ( simpleFloat )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.Tabbed 
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 2 
myModMask       = mod4Mask
  
-- myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]
myWorkspaces    = ["\63083", "\xf268", "\xe7c5", "\xe795", "\xfa7b", "\xf200", "\xe235", "\xfa6f", "\xf49c"]


-- OneDark colorscheme 
myTabConfig = def { activeColor = "#b6bdca"
                  , inactiveColor = "#c8ccd4"
                  , urgentColor = "#abb2bf"
                  , activeBorderColor = "#e5c07b"
                  , inactiveBorderColor = "#d19a66"
                  , urgentBorderColor = "#51afef"
                  , activeTextColor = "#c8ccd4"
                  , inactiveTextColor = "#98c379"
                  , urgentTextColor = "#e06c75"
                  , fontName = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"}


-- myWorkspaces = ["\61704", "\63288", "\xe62b", "\61752", "\61911", "\xfad9", "\xf200", "\64123", "\64109"]

-- myScratchpads
myScratchPads = 
     [ NS "terminal" (myTerminal ++ " --class scratchpad") (resource =? "kitty") myPosition
     , NS "music" (myTerminal ++ " --class music -e cmus") (resource =? "music") myPosition
     , NS "bpytop"  (myTerminal ++ " --class bpytop -e bpytop") (resource =? "bpytop") myPosition
 ] where myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)

myNormalBorderColor  = "#d19a66"
myFocusedBorderColor = "#98c375"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return          ), spawn $ XMonad.terminal conf)


    , ((0,        xF86XK_WWW                   ), spawn "google-chrome-stable")
    , ((mod1Mask,          xK_f                ), spawn "thunar")
    , ((modm,              xK_c                ), spawn "calibre") 
    , ((0,        xF86XK_Mail                  ), spawn "evolution")
    
    , ((modm .|. mod1Mask, xK_f                ), sendMessage $ Toggle NBFULL)
    -- lock screen
    , ((modm,              xK_F1               ), spawn "~/bin/betterlockscreen -l blur") 

    -- launch rofi and htop 
    , ((0,        xF86XK_Search                ), spawn "google-chrome-stable")
    , ((modm,               xK_o               ), spawn "~/bin/launcher.sh")
    , ((modm,               xK_p               ), spawn "kitty -e htop")

    -- Task management via todo-txt and rofi. 
    , ((modm,               xK_d               ), spawn "exec ~/bin/dofi")

    -- Launch slack / lightcord
    , ((modm,               xK_s               ), spawn "slack")
    , ((modm .|. mod1Mask,  xK_d               ), spawn "lightcord")

    -- Audio keys
    , ((0,         xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,         xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,         xF86XK_AudioNext), spawn "playerctl next")
    
    , ((0,        xF86XK_AudioRaiseVolume      ), spawn "amixer sset Master 5%+")
    , ((0,        xF86XK_AudioLowerVolume      ), spawn "amixer sset Master 5%-")
    , ((0,        xF86XK_AudioMute             ), spawn "amixer sset Master toggle")

    -- Brightness keys
    --, ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
--    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
    
    -- ScratchPads mod1mask = ALT key. 
    , ((mod1Mask,   xK_m              ), namedScratchpadAction myScratchPads "music")
    , ((mod1Mask,   xK_t              ), namedScratchpadAction myScratchPads "terminal")
    , ((mod1Mask,   xK_b              ), namedScratchpadAction myScratchPads "bpytop")

    -- Screenshot 
    , ((0,         xK_Print                    ), spawn "exec ~/bin/rofi-screenshot")
    , ((modm,      xK_Print                    ), spawn "exec ~/bin/screenrec.sh")
    , ((modm .|. controlMask, xK_Print         ), spawn "pkill ffmpeg")
    --, ((modm,      xK_v                      ), spawn "exec ~/bin/visualizer")
  
    -- Bitwarden 
    , ((modm,      xK_b                        ), spawn "bitwarden-desktop") 

    -- Minimize stuff

   -- Enable/disable xautolock 
    , ((modm,                xK_z              ), spawn "exec ~/bin/inhibit_activate")
    , ((modm .|. shiftMask,  xK_z              ), spawn "exec ~/bin/inhibit_deactivate")

    -- close focused window
    , ((modm,                xK_x              ), kill)

    --Toggle Gaps.  
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,30), (D,40)]) -- reset the GapSpec

    
    , ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
    , ((modm .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    
    , ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
    , ((modm .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)           -- decrement the top gap
    
    , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
    , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

    , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
    , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

     -- Cycle through layout algorithms. 
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next/prev workspace
    , ((modm,               xK_Right   ), nextWS)
    , ((modm,               xK_Left    ), prevWS)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "xfce4-session-logout")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile ; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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

myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ avoidStruts ( defaultFancyBorders simpleFloat ||| tabs ||| defaultFancyBorders tiled ||| defaultFancyBorders (Mirror tiled) ||| ThreeColMid 1 (3/100) (1/2))


-- avoidStruts( defaultFancyBorders tiled ||| defaultFancyBorders simpleFloat ||| tabs)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1 

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     tabs    = tabbed shrinkText myTabConfig

    -- centerMaster = centerMaster (tiled ||| Tall)

myManageHook = fullscreenManageHook <+> manageDocks <+> manageSpawn <+> namedScratchpadManageHook myScratchPads <+> composeAll
    [ className =? "zoom" --> doFloat
    , className =? "dunst"  --> doFloat 
    , className =? "gimp-2.10" --> doFloat
    , className =? "Spotify" --> doFloat
    , className =? "Picture-in-Picture" --> doFloat
    , className =? "Blueman-manager" --> doCenterFloat  
    , className =? "Pavucontrol" --> doCenterFloat
    , title =? "Panel Preferences" --> doCenterFloat
    , className =? "Thunar" --> doFloat
    , className =? "calibre" --> doFloat
    , className =? "Bitwarden" --> doFloat 
    , className =? "MEGAsync" --> doFloat 
    , className =? "Evolution-alarm-notify" --> doFloat 
    , className =? "Evolution" --> doFloat
    , className =? "calibre" --> doShift ( myWorkspaces !! 3 )
    , className =? "firefox" --> doShift ( myWorkspaces !! 1)
    , className =? "deluge" --> doShift ( myWorkspaces !! 3 )
    , className =? "lightcord" --> doShift ( myWorkspaces !! 4 )
    , className =? "zoom" --> doShift ( myWorkspaces !! 7 )
    , className =? "Virt-manager" --> doCenterFloat 
    , className =? "Virt-manager" --> doShift ( myWorkspaces !! 8 )
    , className =? "Anaconda-Navigator" --> doFloat
    , className =? "Anaconda-Navigator" --> doShift ( myWorkspaces !! 6 )
    , className =? "Vlc" --> doFloat 
    , className =? "Vlc" --> doShift ( myWorkspaces !! 4 ) --  
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop" --> doIgnore
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , resource =? "bpytop" --> doCenterFloat
    , resource =? "music" --> doCenterFloat
                                 ]    -- !!!! THESE ALL SHIFT +1 SO 6 GOES TO 7. 

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty 


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn "exec ~/bin/lock.sh"
  spawnOnce "blueman-applet &"
  spawnOnce "nm-applet &"
  spawnOnce "greenclip daemon"
  spawnOnce "picom -fb"
  spawnOnce "dunst"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "nitrogen --restore"
  spawnOnce "~/.config/polybar/xmonad-launch.sh" 
  spawnOnce "setxkbmap -option ctrl:nocaps "
  spawnOnce "xcape -e 'Control_L=Escape'"
  spawnOnce "xcape -e 'Super_L=Super_L|space'"
  
  -- spawnOnce "xfce4-panel &"
  spawnOnce "numlockx &"
  setWMName "LG3D"
 -- ewmhDesktopsStartup

-- Run xmonad with the settings you specify. No need to modify this.

main :: IO ()
main = do 
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
       [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


--    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad $ docks $ ewmh defaults

defaults = def {

      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,  
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook, 
        layoutHook = gaps [(L,20), (R,20), (U,30), (D,30)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,          
        startupHook        = myStartupHook >> addEWMHFullscreen}

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["Welcome to electr0n's xmonad setup. Default modifier'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch kitty",
    "mod-o            Launch rofi",
    "mod-Shift-p      Launch gmrun",
    "mod-x            Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
