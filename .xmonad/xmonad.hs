{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--     ___ ___  ___ ___ _______ ______  _______ ______
--    (   Y   )|   Y   |   _   |   _  \|   _   |   _  \
--     \  1  / |.      |.  |   |.  |   |.  1   |.  |   \
--     /  _  \ |. \_/  |.  |   |.  |   |.  _   |.  |    \
--    /:  |   \|:  |   |:  1   |:  |   |:  |   |:  1    /
--   (::. |:.  |::.|:. |::.. . |::.|   |::.|:. |::.. . /
--    `--- ---'`--- ---`-------`--- ---`--- ---`------'
--              agonzal @ github / dotfiles.git
--
--
--
--
--
--
--
--
--
--
--
--

-- Polybar Integration Imports

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (forM_, join, when)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Function (on)
import Data.List (isSuffixOf, sortBy)
import qualified Data.Map as M
import Data.Maybe (maybeToList, isJust)
import Data.Monoid ()

-- ( dynamicLogWithPP, wrap, PP(..), shorten )

-- FancyBorders (double borders)
import FancyBorders


import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPlay,
    xF86XK_AudioPrev,
    xF86XK_AudioRaiseVolume,
    xF86XK_Calculator,
    xF86XK_HomePage,
    xF86XK_Mail,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
    xF86XK_MyComputer,
    xF86XK_Search,
    xF86XK_WWW,
  )
import System.Exit ()
import System.IO
import XMonad


-- XMonad Actions

import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.TagWindows 
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Minimize
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (killAll)


-------------------------------Minimize / DynamicLog 
-- XMonad Hooks --------------
-------------------------------
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsEventHook, ewmhDesktopsEventHookCustom, ewmhDesktopsLogHook, ewmhDesktopsStartup)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
  ( Direction2D (D, L, R, U),
    ToggleStruts (..),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.Minimize
import XMonad.Hooks.RestoreMinimized 
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog 

-- XMonad Layouts 

import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
  ( fullscreenEventHook,
    fullscreenFull,
    fullscreenManageHook,
    fullscreenSupport,
  )
import XMonad.Layout.Gaps
  ( Direction2D (D, L, R, U),
    GapMessage (DecGap, IncGap, ToggleGaps),
    gaps,
    setGaps,
  )
import XMonad.Layout.Minimize 
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (..))
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle), ToggleLayouts)
import qualified XMonad.Layout.BoringWindows as B


import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspace, namedScratchpadManageHook)
import XMonad.Util.NamedWindows (NamedWindow, getName)
import XMonad.Util.Run (runInTerm, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Types
import XMonad.Util.WorkspaceCompare (getSortByIndex, getSortByTag)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "kitty"

myNormalBorderColor = "#abb2bf"

myFocusedBorderColor = "#c8ccd4"

myActiveBorderColor = "#c8ccd4"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 1

myModMask = mod4Mask

-- myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]
myWorkspaces = ["\xf120", "\xf268", "\xe7c5", "\xfad9", "\xf292", "\xf200", "\xe235", "\xfa6f", "\xf49c"]

-- OneDark colorscheme
myTabConfig =
  def
    { activeColor = "#1e222a",
      inactiveColor = "#565a64",
      urgentColor = "#e06c75",
      activeBorderColor = "#1e222a",
      inactiveBorderColor = "#565a64",
      urgentBorderColor = "#e06c75",

      activeTextColor = "#abb2bf",
      inactiveTextColor = "#c8ccd4",
      urgentTextColor = "#e5c07b",
      fontName = "xft:JetBrainsMono Nerd Font:size=10:antialias=true",
      decoHeight = 15
    }

-- standardTheme used with SideDecoration module.

standardTheme =
  def
    { activeColor = "#b6bdca",
      activeBorderColor = "#e5c07b",
      activeTextColor = "#c8ccd4",
      inactiveBorderColor = "#d19a66",
      inactiveColor = "#565c64",
      inactiveTextColor = "#98c379",
      urgentBorderColor = "#51afef",
      urgentColor = "#e06c75",
      urgentTextColor = "#e5c07b",
      decoWidth = 4,
      decoHeight = 2
    }

-- SideDecorations

data SideDecoration a = SideDecoration Direction2D deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing

-- myScratchpads
myScratchPads =
  [ NS "terminal" (myTerminal ++ " --class scratchpad") (resource =? "scratchpad") myPosition,
    NS "music" (myTerminal ++ " --class music -e ncmpcpp -c ~/.ncmpcpp/config") (resource =? "music") myPosition,
    NS "glance" (myTerminal ++ " --class glance -e glances") (resource =? "glance") myPosition,
    NS "ranger" (myTerminal ++ " --class ranger -e ranger") (className =? "ranger") myPosition
  ]
  where
    myPosition = customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 2) (1 / 2)

-----------------------------------------
-- Floating functions
-----------------------------------------
centerRect = W.RationalRect 0.25 0.25 0.5 0.5

-- Center and float a window (retain size of tile)
centerFloat win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
  return ()

-- Float a window in the center (1/4 of the screen)
centerFloat' w = windows $ W.float w centerRect

-- Make a window my default size (half of the screen) keeping the center of the window fixed
customFloat win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect 0.5 0.5 0.5 0.5)
  return ()

-- Float and place bottom right a tiled window, sink a floating window
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect (1 / 3) (2 / 4) (1 / 2) (2 / 5)) s)
    )

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
    [ ((shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      ((modm .|. shiftMask, xK_Return), spawn "/usr/local/bin/st"),
      
      -- Dropdowns
      ((mod1Mask, xK_m), spawn "tdrop --wm xmonad -n 3 -h 80% -w 60% -x 20% -y 10% kitty --class mail -e aerc"),
      ((mod1Mask, xK_t), spawn "tdrop --wm xmonad -n 1 -h 40% -w 60% -x 20% kitty --class scratchterm"),
      ((mod1Mask, xK_r), spawn "tdrop --wm xmonad -n 2 -w 30% -h 70% -y 20% kitty --class ranger -e ranger"),
      ((mod1Mask, xK_v), spawn "tdrop --wm xmonad -n 4 -w 60% -h 70% -x 20% -y 20% kitty --class nvim -e nvim +Dashboard"),
      ((mod1Mask, xK_f), spawn "tdrop --wm xmonad -w 30% -h 70% thunar"),
      ((mod1Mask, xK_b), spawn "tdrop --wm xmonad -w 29% -h 70% -x 70% st -c dropdown -e bpytop"),

      -- Media function keys (No Modifier)
      ((0, xF86XK_WWW), spawn "google-chrome-stable"), -- laptop config
      ((0, xF86XK_MyComputer), spawn "thunar"), -- doesm't work ?
      ((0, xF86XK_Mail), spawn "tdrop --wm xmonad -n 3 -w 60% -h 80% -x 20% -y 10% kitty --class mail -e aerc"),
      ((0, xF86XK_HomePage), spawn "google-chrome-stable"),
      ((0, xF86XK_Search), spawn "thunar"),
      
      -- toggleFloats (Super / supert+shift)
      ((modm, xK_f), withFocused toggleFloat),
      ((modm .|. shiftMask, xK_f), withFocused customFloat),
      ((modm, xK_c), withFocused centerFloat),
      ((modm .|. shiftMask, xK_c), withFocused centerFloat'),
      
      -- lock screen Super + ALT + L
      ((modm .|. mod1Mask, xK_l), spawn "betterlockscreen -l dimpixel"),
      
      -- Launch rofi enabled scripts
      ((modm, xK_o), spawn "~/bin/launcher.sh"), -- rofi + windowcd / file / clipboard
      ((modm, xK_p), spawn "bwmenu"), -- bitwarden rofi passwd management
      ((modm, xK_t), spawn "~/bin/dofi"), -- task management

      
      -- Chats: Super + Alt
      ((modm .|. mod1Mask, xK_s), spawn "slack"),
      ((modm .|. mod1Mask, xK_d), spawn "lightcord"),
      ((modm .|. mod1Mask, xK_t), spawn "telegram-desktop"),
      
      -- Audio keys (No Modifier)
      ((0, xF86XK_AudioPlay), spawn "mpc toggle"),
      ((0, xF86XK_AudioPrev), spawn "mpc prev"),
      ((0, xF86XK_AudioNext), spawn "mpc next"),
      ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+"),
      ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-"),
      ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle"),
      
      -- Brightness keys
      ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%"),
      
      -- ScratchPads (Modifier: ALT key)
      -- ((mod1Mask, xK_m), namedScratchpadAction myScratchPads "music"),
      -- ((mod1Mask, xK_t), namedScratchpadAction myScratchPads "terminal"),
      -- ((mod1Mask, xK_r), namedScratchpadAction myScratchPads "ranger"),
      
      -- Floating windows management (control + shift + direction)
      ((controlMask .|. shiftMask, xK_Left), withFocused (keysMoveWindow (-10, 0))),
      ((controlMask .|. shiftMask, xK_Up), withFocused (keysMoveWindow (0, -10))),
      ((controlMask .|. shiftMask, xK_Down), withFocused (keysMoveWindow (0, 10))),
      ((controlMask .|. shiftMask, xK_Right), withFocused (keysMoveWindow (10, 0))),
      
      -- Floating windows Resizing (control + alt)
      ((controlMask .|. mod1Mask, xK_Left), withFocused (keysResizeWindow (-10, 0) (0, 0))),
      ((controlMask .|. mod1Mask, xK_Up), withFocused (keysResizeWindow (0, -10) (0, 0))),
      ((controlMask .|. mod1Mask, xK_Down), withFocused (keysResizeWindow (0, 10) (0, 0))),
      ((controlMask .|. mod1Mask, xK_Right), withFocused (keysResizeWindow (10, 0) (0, 0))),
      ((controlMask .|. mod1Mask, xK_Page_Down), withFocused (keysAbsResizeWindow (10, 10) (0, 0))),
      ((controlMask .|. mod1Mask, xK_Page_Up), withFocused (keysAbsResizeWindow (-10, -10) (0, 0))),
      
      -- ResizableTall
      ((modm, xK_Down), sendMessage MirrorShrink),
      ((modm, xK_Up), sendMessage MirrorExpand),
      
      -- Screenshot
      ((0, xK_Print), spawn "maim ~/pics/scrnshts/main-screenshot.png ; notify-send -a maim 'screenshot' 'screenshot saved to ~/pics/scrnshts'"),
      ((shiftMask, xK_Print), spawn "rofi-screenshot"),
      ((modm, xK_Print), spawn "exec ~/bin/screenrec.sh"),
      ((modm .|. controlMask, xK_Print), spawn "pkill ffmpeg"),
      
      -- Visualizer
      ((modm, xK_v), spawn "exec ~/bin/visualizer"),
      
      -- Bitwarden
      ((modm .|. shiftMask, xK_b), spawn "bitwarden-desktop"),
      
      -- Minimize window ( super + alt )
      ((modm .|. mod1Mask, xK_m), withFocused minimizeWindow), -- polywins handles maximize for my setup.
     
      
      -- Enable/disable xautolock
      ((modm, xK_a), spawn "exec ~/bin/enable_autolock"),
      ((modm .|. shiftMask, xK_a), spawn "exec ~/bin/disable_autolock"),
      
      -- close focused window / kill all ws windows
      ((modm, xK_x), kill),
      ((modm .|. mod1Mask, xK_x), killAll),
      
      --Toggle Gaps.
      ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps), -- toggle all gaps
      ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L, 10), (R, 10), (U, 5), (D, 40)]), -- reset the GapSpec
      ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L), -- increment the left-hand gap
      ((modm .|. shiftMask, xK_t), sendMessage $ DecGap 10 L), -- decrement the left-hand gap
      ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U), -- increment the top gap
      ((modm .|. shiftMask, xK_y), sendMessage $ DecGap 10 U), -- decrement the top gap
      ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D), -- increment the bottom gap
      ((modm .|. shiftMask, xK_u), sendMessage $ DecGap 10 D), -- decrement the bottom gap
      ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R), -- increment the right-hand gap
      ((modm .|. shiftMask, xK_i), sendMessage $ DecGap 10 R), -- decrement the right-hand gap

      -- Cycle through layout algorithms.
      ((modm, xK_space), sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- Move focus to the next/prev workspace
      ((modm, xK_Right), nextWS),
      ((modm, xK_Left), prevWS),
      -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp),
      -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modm, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_l), sendMessage Expand),
      -- Push window back into tiling
      -- , ((modm,                  xK_t                         ), withFocused $ windows . W.sink)

      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      ((modm, xK_b), withFocused toggleBorder),
      -- Quit xmonad
      ((modm .|. shiftMask, xK_q), spawn "logout"), -- kill -9 -1

      -- Restart xmonad
      ((modm, xK_q), spawn "xmonad --recompile ; xmonad --restart"),
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        ( \w ->
            focus w >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w >> Flex.mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = minimize $ B.boringWindows $ defaultFancyBorders (avoidStruts ( centered |||  tiled ||| Mirror (Mirror tiled) ||| tabs))
  where
    -- avoidStruts( defaultFancyBorders tiled ||| defaultFancyBorders simpleFloat ||| tabs)

    -- default tiling algorithm partitions the screen into two panes
    tiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 2 / 3

    -- Percent of screen to increment by when resizing panes
    delta = 1 / 100

    tabs = tabbedBottom shrinkText myTabConfig

    centered = ThreeColMid 1 (3 / 100) (1 / 2)

    --sidedecor = decoration shrinkText standardTheme (SideDecoration R)

myManageHook =
  fullscreenManageHook <+> manageDocks <+> manageSpawn <+> namedScratchpadManageHook myScratchPads
    <+> composeAll
      [ className =? "zoom" --> doFloat,
        className =? "dunst" --> doFloat,
        className =? "Gimp-2.10" --> doFloat,
        className =? "St" --> doCenterFloat,
        className =? "dropdown" --> doFloat,
        className =? "Picture-in-Picture" --> doFloat,
        className =? "Blueman-manager" --> doCenterFloat,
        className =? "Pavucontrol" --> doCenterFloat,
        className =? "Thunar" --> doFloat,
        className =? "calibre" --> doFloat,
        className =? "Bitwarden" --> doFloat,
        className =? "Uget-gtk" --> doCenterFloat,
        className =? "Pavucontrol" --> doCenterFloat,
        className =? "kitty" --> doCenterFloat,
        className =? "nvim" --> doFloat, 
        className =? "scratchterm" --> doFloat,
        className =? "mail" --> doFloat,
        className =? "SimpleScreenRecorder" --> doCenterFloat,
        className =? "calibre" --> doShift (myWorkspaces !! 3),
        className =? "Google-chrome" --> doShift (myWorkspaces !! 1),
        className =? "deluge" --> doShift (myWorkspaces !! 3),
        className =? "Lightcord" --> doShift (myWorkspaces !! 4),
        className =? "TelegramDesktop" --> doShift (myWorkspaces !! 4),
        className =? "Virt-manager" --> doCenterFloat,
        className =? "Virt-manager" --> doShift (myWorkspaces !! 7),
        className =? "Vlc" --> doFloat,
        resource =? "desktop_window" --> doIgnore,
        resource =? "kdesktop" --> doIgnore,
        isFullscreen --> doFullFloat,
        isDialog --> doCenterFloat,
        resource =? "music" --> doCenterFloat
      ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- Emit a DBus signal on log updates

-- myLogHook :: D.Client -> PP
-- myLogHook dbus = def
--     { ppOutput = dbusOutput dbus
--     , ppCurrent = wrap ("%{F" ++ "#98c370" ++ "} ") " %{F-}"
--     , ppVisible = wrap ("%{B" ++ "#2b2f37" ++ "} ") " %{B-}"
--     , ppUrgent = wrap ("%{F" ++ "#d19a66" ++ "} ") " %{F-}"
--     , ppHidden = wrap " " " "
--     , ppWsSep = ""
--     , ppSep = " : "
--     , ppTitle = shorten 10
--     }
-- 
--I use a case statement to explicitly give the Orderings because I want to treat
--the Nothing case opposite of how it's treated with compare by default

eventLogHookForPolyBar = do
    winset <- gets windowset
    title <- maybe (return "") (fmap show . getName) . W.peek $ winset
    let currWs = W.currentTag winset
    let wss = map W.tag $ W.workspaces winset

    io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
    io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr currWs wss ++ "\n")

    fadeInactiveLogHook 0.7
    where
      fmt currWs ws
            | currWs == ws = "[" ++ ws ++ " ]"
            | otherwise    = " " ++ ws ++ " "
      wsStr currWs wss = join $ map (fmt currWs) $ sortBy (compare `on` (!! 0)) wss

--myLogHook :: X ()
--myLogHook =
--
--  let noScratchpad ws = if ws == "NSP" then "" else ws
--
--  in  do
--        updatePointer (0.5, 0.5) (0, 0)
--------------------------------------------------------------------------
---- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn "exec ~/bin/autolock.sh"
  spawn "exec ~/bin/layout.sh"
  spawn "exec ~/.config/polybar/launch.sh xmonad"
  spawn "blueman-applet"
  spawn "nitrogen --restore"
  spawn "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "nm-applet"
  spawn "greenclip daemon"
  spawnOnce "volumeicon"
  spawnOnce "picom -fb"
  spawnOnce "mpd ~/.mpd/mpd.conf"
  spawnOnce "dunst"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawn "xset -dpms s off"
  spawnOnce "setxkbmap -option ctrl:nocaps "
  spawnOnce "xcape -e 'Control_L=Escape'"
  spawnOnce "xcape -e 'Super_L=Super_L|o'"
  setWMName "LG3D"

-- Run xmonad with the settings you specify. No need to modify this.


     --dbus <- D.connectSession
     -- Request access to the DBus name
     --D.requestName dbus (D.busName_ "org.xmonad.Log")
         --[D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
main :: IO ()
main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad $ ewmh $ docks $ fullscreenSupport $ defaults  
 where
  myBar = "polybar"
  
--  xmonad $ fullscreenSupport $ docks $ ewmh defaults

defaults = def 
    { -- simple stuff
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeys,
      mouseBindings = myMouseBindings,
      -- hooks, layouts
      manageHook = myManageHook,
      layoutHook = gaps [(L, 10), (R, 10), (U, 5), (D, 40)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders myLayout,
      handleEventHook = myEventHook <+> restoreMinimizedEventHook <+> minimizeEventHook <+> fullscreenEventHook,
      logHook = eventLogHookForPolyBar,      
      startupHook = myStartupHook >> addEWMHFullscreen
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "Welcome to electr0n's xmonad setup. Default modifier 'super'. Default keybindings:",
      "",
      "-- launching and killing programs",
      "mod-Shift-Enter  Launch kitty",
      "mod-o            Launch rofi",
      "mod-x            Close/kill the focused window",
      "mod-Space        Rotate through the available layout algorithms",
      "mod-Shift-Space  Reset the layouts on the current workSpace to default",
      "mod-n            Resize/refresh viewed windows to the correct size",
      "",
      "-- move focus up or down the window stack",
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
      "-- Gaps (Increase) Ctrl Modifier ",
      "mod-ctrl+t   Increase Left GAP",
      "mod-ctrl-y   Increase Up GAP",
      "mod-ctrl-u   Increase Down GAP",
      "mod-ctrl-i   Increase Right GAP",
      "",
      "-- GAPS (Decrease) Shift modifier",
      "mod-shift-t  Decrase Left GAP",
      "mod-shift-y  Decrease Up GAP",
      "mod-shift-u  Decrease Down GAP",
      "mod-shift-i  Decrease Right GAP",
      "-- floating layer support",
      "mod-t  Push window back into tiling; unfloat and re-tile it",
      "",
      "-- increase or decrease number of windows in the master area",
      "mod-comma  (mod-,)   Increment the number of windows in the master area",
      "mod-period (mod-.)   Deincrement the number of windows in the master area",
      "",
      "-- quit, or restart",
      "mod-Shift-q  Logout (kill -9 -l)",
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
      "mod-button3  Set the window to floating mode and resize by dragging",
      "",
      "-- Toggle Layouts",
      "mod-alt-f   Toggle NBFULL",
      "mod-alt-n   Toggle NOBORDERS",
      "",
      "-- ScratchPads ",
      "ALT + T   Launch myTerminal",
      "ALT + M   Launch ncmpcpp via myTerminal",
      "ALT + B   Launch bpytop via myTerminal",
      "ALT + F   Launch ranger via myTerminal "
    ]
