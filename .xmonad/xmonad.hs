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
--  Imports: Data, Control, Dbus UTF8       Lines:  25  --> 34 
--  Imports: FancyBorders module            Lines   38
--  Imports: Graphics, System, XMonad       Lines:  58  --> 60
--  Imports: Actions                        Lines:  65  --> 74
--  Imports: Hooks                          Lines:  80  --> 94
--  Imports: Layouts                        Lines:  98  --> 120
--  Imports: Utils                          Lines:  124 --> 131
--  Variables:                              Lines:  134 --> 151
--  myModMask:                              Lines:  153
--  myWorkspaces:                           Lines:  156
--  myTabConfig:                            Lines:  164 --> 178
--  standardTheme (sidedecor):              Line:   183
--  SideDecoration                          Lines:  199 --> 215 
--  myScratchPads:                          Lines:  217 (phased out in favor of tdrop). 
--  custom Float funcs:                     Lines:  234 --> 252 
--  addNETSupported / addEWMHFullscreen:    Lines: 261 --> 271
--  myKeys                                  Lines: 279 --> 455
--  myMouseBindings:                        Lines: 459 --> 475 
--  myLayout:                               Lines: 480 --> 499
--  myManageHook:                           Lines: 499 --> 538
--  eventLogHookForPolyBar:                 Lines: 563 --> 577 
--  myStartupHook:                          Lines: 594 --> 611 
--  main:                                   Lines: 621 --> 648 
--  help:                                   Lines: 652 --> 720 

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (forM_, join, when)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Function (on)
import Data.List (isSuffixOf, sortBy)
import qualified Data.Map as M
import Data.Maybe (maybeToList, isJust)
import Data.Monoid ()

-- FancyBorders (double borders)
import FancyBorders

-- Themes
import Themes 

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

------------------------------
-- XMonad Actions ------------
------------------------------

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


-------------------------------
----- XMonad Hooks ------------
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
import XMonad.Layout.SimpleFloat 
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (..))
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.Tabbed
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.SubLayouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle), ToggleLayouts)
import qualified XMonad.Layout.BoringWindows as B

 -- XMonad Util imports 

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
myBrowser  = "google-chrome-stable"

myNormalBorderColor = "#abb2bf"

myFocusedBorderColor = "#abb2bf"

myActiveBorderColor = "#e5c07b"


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True 

-- Width of the window border in pixels.
myBorderWidth = 2 

myModMask = mod4Mask

-- myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]
myWorkspaces = ["\xf120", "\xf268", "\xe7c5", "\xfad9", "\xf292", "\xf200", "\xf040", "\xfa6f", "\xf49c"]

-- OneDark colorscheme
myTabConfig =
  def
    { activeColor = "#1e222a",
      inactiveColor = "#1e222a",
      urgentColor = "#e06c75",
      activeBorderColor = "#1e222a",
      activeBorderWidth = 0,
      inactiveBorderColor = "#98c379",
      inactiveBorderWidth = 2,
      urgentBorderColor = "#e5c07b",

      activeTextColor = "#abb2bf",
      inactiveTextColor = "#abb2bf",
      urgentTextColor = "#e5c07b",
      fontName = "xft:JetBrainsMonoMedium:style=bold:size=6:antialias=true",
      decoHeight = 1 
    }

-- standardTheme used with SideDecoration module.

standardTheme =
  def
    { fontName = "",
      activeColor = "#98c379",
      activeBorderColor = "#98c379",
      activeTextColor = "#98c379",
      inactiveBorderColor = "#98c379",
      inactiveColor = "#98c379",
      inactiveTextColor = "#98c379",
      urgentBorderColor = "#e06c75",
      urgentColor = "#e06c75",
      urgentTextColor = "#e5c07b",
      decoWidth = 5 
    }

-- SideDecorations (https://github.com/xmonad/xmonad/issues/152#issuecomment-362716434)
-- Looks horrible when you do rounded corners with picom. 
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

-- myScratchpads (deprecatd in my setup in favor of tdrop)
-- Only here for others to utilize the setup. 
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
      ((modm .|. shiftMask, xK_Return), spawn "tabbed -n scratchterm  -r 2  -t '#1e222a' -T '#565c64' -u '#1e222a' -U '#565c64' st -w '' -c scratchterm  -e zsh"),

      -- Dropdowns
      ((mod1Mask, xK_m), spawn "tdrop --wm xmonad -n 1 -h 80% -w 60% -x 20% -y 10% kitty --class mail -e aerc"),
      -- ((mod1Mask, xK_t), spawn "tdrop --wm xmonad -n 2 -h 80% -w 60% -x 20% -y 10% tabbed -n scratchterm -r 2 -t '#1e222a' -T '#565c64' -u '#1e222a' -U '#565c64' st -w '' -c scratchterm -e zsh"),
      ((mod1Mask, xK_t), spawn "tdrop --wm xmonad -n 2 -h 60% -w 60% -x 20% -y 10% kitty --class scratchterm"),
      ((mod1Mask, xK_i), spawn "tdrop --wm xmonad -n 3 -h 60% -w 60% -x 20% kitty --class irc -e weechat"),
      ((mod1Mask, xK_r), spawn "tdrop --wm xmonad -n 4 -w 80% -h 40% -y 55% -x 10% kitty --class ranger -e ranger"),
      ((mod1Mask, xK_e), spawn "tdrop --wm xmonad -n 5 -w 60% -h 70% -x 20% -y 20% kitty --class nvim -e nvim"),
      ((mod1Mask, xK_g), spawn "tdrop --wm xmonad -w 60% -h 80% -x 20% -y 10% goneovim"),
      ((mod1Mask, xK_f), spawn "tdrop --wm xmonad -w 30% -h 70% thunar"),
      ((mod1Mask, xK_b), spawn "tdrop --wm xmonad -w 29% -h 70% -x 70% st -c dropdown -e btop"),

      -- Media function keys (No Modifier)
      ((0, xF86XK_WWW), spawn myBrowser), -- laptop config
      ((0, xF86XK_MyComputer), spawn "thunar"), -- doesm't work ?
      ((0, xF86XK_Mail), spawn "tdrop --wm xmonad -n 3 -w 60% -h 80% -x 20% -y 10% kitty --class mail -e aerc"),
      ((0, xF86XK_HomePage), spawn myBrowser),
      ((0, xF86XK_Search), spawn "thunar"),
      ((0, xF86XK_Calculator), spawn "galculator"),

      -- toggleFloats (Super / supert+shift)
      ((modm, xK_f), withFocused toggleFloat),
      ((modm .|. shiftMask, xK_f), withFocused customFloat),
      ((modm, xK_c), withFocused centerFloat),
      ((modm .|. shiftMask, xK_c), withFocused centerFloat'),

      -- lock screen Super + ALT + L
      ((modm .|. mod1Mask, xK_l), spawn "~/bin/lockscreen"),

     -- Enable/disable xautolock
      ((modm, xK_a), spawn "exec ~/bin/enable_autolock"),
      ((modm .|. shiftMask, xK_a), spawn "exec ~/bin/disable_autolock"),

      -- Launch rofi enabled scripts
      ((modm, xK_o), spawn "~/bin/launcher.sh"), -- rofi + windowcd / file / clipboard
      ((modm, xK_p), spawn "bwmenu"), -- bitwarden rofi passwd management
      ((modm, xK_t), spawn "~/bin/dofi"), -- task management


      -- Chats: Super + Alt
      ((modm .|. mod1Mask, xK_c), spawn "calibre"),
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
      ((0, xK_Print), spawn "screenshot.sh"),                               -- full screenshot 
      ((shiftMask, xK_Print), spawn "screenshot.sh -s"),                    -- area/window to FILE
      ((controlMask, xK_Print), spawn "sreenshot.sh -c"),                   -- area/window to CLIP
      ((modm, xK_Print), spawn "exec ~/bin/screenrec.sh"),                  -- screnrec via ffmpeg 
      ((modm .|. controlMask, xK_Print), spawn "pkill ffmpeg"),
      
      -- Visualizer
      ((modm, xK_v), spawn "exec ~/bin/visualizer"),
      
      -- Bitwarden
      ((modm .|. shiftMask, xK_b), spawn "bitwarden-desktop"),
      
      -- Minimize window ( super + alt )
      ((modm .|. mod1Mask, xK_m), withFocused minimizeWindow), -- polywins handles maximize for my setup.
      ((mod1Mask .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus),
            
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

      -- SubLayouts 
      ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L),
      ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R),
      ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U),
      ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D),

      ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll)),
      ((modm .|. mod1Mask, xK_u), withFocused (sendMessage . UnMerge)),

      ((modm .|. controlMask, xK_period), onGroup W.focusUp'),
      ((modm .|. controlMask, xK_comma), onGroup W.focusDown'),

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
     --((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
     ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | zenity --width 600 --height 1200 --text-info -"))
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

myLayout = minimize $ subTabbed $ B.boringWindows $ defaultFancyBorders (avoidStruts ( centered |||  tiled ||| tabs ))
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

    tabs = tabbedBottom shrinkText (theme oneDarkTheme) 

    centered = ThreeColMid 1 (3 / 100) (1 / 2)

    -- sidedecor = decoration shrinkText standardTheme (SideDecoration L)

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
        className =? "Pcmanfm" --> doFloat,
        className =? "Bitwarden" --> doFloat,
        className =? "Uget-gtk" --> doCenterFloat,
        className =? "Pavucontrol" --> doCenterFloat,
        className =? "kitty" --> doCenterFloat,
        className =? "nvim" --> doFloat, 
        className =? "tabbed" --> doCenterFloat,
        className =? "scratchterm" --> doFloat,
        className =? "irc" --> doFloat,
        className =? "goneovim" --> doFloat,
        className =? "mail" --> doFloat,
        className =? "SimpleScreenRecorder" --> doCenterFloat,
        className =? "Transmission-gtk" --> doCenterFloat,
        className =? "obsidian" --> (doCenterFloat <+> doShift (myWorkspaces !! 6)),
        className =? "calibre" --> (doCenterFloat <+> doShift (myWorkspaces !! 3)),
        className =? "Google-chrome" --> doShift (myWorkspaces !! 1),
        className =? "deluge" --> (doCenterFloat <+> doShift (myWorkspaces !! 5)),
        className =? "Slack" --> doShift (myWorkspaces !! 4),
        className =? "Lightcord" --> doShift (myWorkspaces !! 4),
        className =? "TelegramDesktop" --> doShift (myWorkspaces !! 4),
        className =? "Virt-manager" --> (doCenterFloat <+> doShift (myWorkspaces !! 7)),
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

--I use a case statement to explicitly give the Orderings because I want to treat
--the Nothing case opposite of how it's treated with compare by default

eventLogHookForPolyBar = do
    winset <- gets windowset
    title <- maybe (return "") (fmap show . getName) . W.peek $ winset
    let currWs = W.currentTag winset
    let wss = map W.tag $ W.workspaces winset

    io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
    io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr currWs wss ++ "\n")

    where
      fmt currWs ws
            | currWs == ws = "[" ++ ws ++ " ]"
            | otherwise    = " " ++ ws ++ " "
      wsStr currWs wss = join $ map (fmt currWs) $ sortBy (compare `on` (!! 0)) wss

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.7
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
  spawn "~/bin/autolock.sh &"
  spawn "blueman-applet &"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
  spawnOnce "nm-applet &"
  spawnOnce "greenclip daemon"
  spawnOnce "volumeicon &"
  spawnOnce "picom -fb"
  spawnOnce "dunst"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawn "xset -dpms s off"
  spawn "setxkbmap -option ctrl:nocaps "
  spawnOnce "xcape -e 'Control_L=Escape'"
  spawnOnce "xcape -e 'Super_L=Super_L|o'"
  spawn "~/.config/polybar/launch.sh xmonad"
  spawn "telegram-desktop"
  spawn "nitrogen --restore"
  spawn myBrowser 
  setWMName "LG3D"
  ewmhDesktopsStartup

-- Run xmonad with the settings you specify. No need to modify this.


     --dbus <- D.connectSession
     -- Request access to the DBus name
     --D.requestName dbus (D.busName_ "org.xmonad.Log")
         --[D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

main :: IO ()
main = mkDbusClient>>= main' 

main' :: D.Client -> IO ()
main' dbus = xmonad $ ewmh $ docks $ fullscreenSupport $ defaults  
 where
  myBar = "polybar -c ~/.config/polybar/config.ini -r"
  
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
      layoutHook = gaps [(L, 10), (R, 10), (U, 5), (D,45)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders myLayout,
      handleEventHook = myEventHook <+> restoreMinimizedEventHook <+> minimizeEventHook <+> fullscreenEventHook,
      logHook = myLogHook >> eventLogHookForPolyBar,      
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
      "",
      "-- CUSTOM floating layer support",
      "mod-f            float or sink current focused window",
      "mod-c            float current focused window (keep same size as tiled)",
      "mod-shift-f      float current focused window + resize 1/4 of screen + move to bottom right",
      "mod-shift-c      float current focused window + doCenterFloat",
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
      "-- ScratchPads via tdrop ",
      "ALT + T   Launch myTerminal",
      "ALT + M   Launch aerc via myTerminal",
      "ALT + B   Launch bpytop via st",
      "ALT + R   Launch ranger via myTerminal",
      "ALT + I   Launch weechat via myTerminal",
      "ALT + F   Launch Thunar",
      "ALT + E   Launch neovim via myTermainl",
      "ALT + G   Launch Goneovim",
      "",
      "Using tdrop as a drop-in replacement for standard scratchpads on xmonad. Each tdrop class has manageHooks for", 
      "floating and I set custom locations + height and width for each window. Invoking bpytop will appear on the top",
      " right like a true drop-down terminal etc... "
    ]
