     ___ ___  ___ ___ _______ ______  _______ ______   
    (   Y   )|   Y   |   _   |   _  \|   _   |   _  \  
     \  1  / |.      |.  |   |.  |   |.  1   |.  |   \ 
     /  _  \ |. \_/  |.  |   |.  |   |.  _   |.  |    \
    /:  |   \|:  |   |:  1   |:  |   |:  |   |:  1    /
   (::. |:.  |::.|:. |::.. . |::.|   |::.|:. |::.. . / 
    `--- ---'`--- ---`-------`--- ---`--- ---`------'  
                   -=-=-=-=-=-=-=-=-=- haskell config 


## Config overview 

 - I use 'myTerminal' variable to define kitty as my terminal emulator. This variable is used with the keybinding and scratchPads. 
 - We declare types Bool for myClockJustFocuses and set false and myFocusFollowsMouse and set true. 
 - myBorderWidth = 2 (colors match OneDark) 
 - myModMask = mod4Mask (Super aka Windows)
 - 9 workspaces defined using their HEX code from nerd-fonts. 
   - Term | Chrome | Vim | Reading | Slack | Visualizations | DataSci | Droplets | Analysis 


### ScartchPads 

If I had more use=cases for them, I would utilize more scratchpads. TODO: hide NSP workspace from polybar, maybe using ewmhDesktopsEventHookCustom ? 

- ScratchPads for :
   - kitty (with --class scratchpad)
   - ncmpcpp 
   - bpytop 
   - thunar 
 
 ScartchPads utilize the ALT + {t,m,b,f} depending on which one you want to summon. ScartchPads utilize myPositiion which basically is a centerFloat via W.RationalRect (1/3) (1/3) (1/3) (1/3)

### Layouts 

 My configuration utilizes the following layouts:

 - simpleFloat 
 - tabs 
 - tiled 
 - Mirror tiled
 - centered (ThreeColMid)

  You can find config for tab layout as myTabConfig on Line 91. 

  Layouts are defined with myLayout which utilizes mkToggle from XMonad.Layout.MultiToggle. Allows for keybindings to toggle NBFULL and NOBORDERS. then the defined layouts
  are wrapped in avoidstruts from XMonad.Hooks.ManageDocks and of course XMonad.Layout.Gaps which are L 10 R 10 U 10 D 30. 

### ManageHook

  myManageHook has definitions for fullscreen support, manageDocks, manageSpawns and namedScatchPadManageHook. I also heavily utilize className / resource / title for 
  automatic window rules and placement. Lines 341 to 371. I need to rework this section to use variables that way I can have 3 to 5 defintions for each variable group, will 
  result in much cleaner config. 

### StartupHook

 This section invokes ~/bin/lock.sh for xautolock and ~/.xmonad/autostart.sh to run the various components needed for my workspace such as:

  - numlockx 
  - nm=applet 
  - pa-applet
  - volumeicon 
  - gnome-polkit 
  - setxkbmap 
  - xcape 
  - picom 
  - feh (set bg)
  - xsetroot for mouse pointer 

 I also set setWMName to LG3D as a work-around to get CobaltStrike and other Java based applications to work. 

### main 

 I do have dbus awareness here but I am not currently using this as it was easier to get polybar integrated just using ewmh module without xmonad-log, dbus, etc... 



 I call xmonad via 'xmonad % docks $ ewmh defaults '

 You can find defaults defined at line 422: 


StartupHook = myStartupHook >> addEWMHFullscreen calls this for actual fullscreen support as using XMonad.Layout.Fullscreen would make certain apps fullscreen WITHIN the tiled dimensions (I am looking at you chrome)

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]


HELP => Press Super + Shift + Front Slash (aka ?) to get an xmessage with default XMonad keybindings. Does not currently include keybindings for individual apps that i have specified but
that can all be solved by invoking Super + O to exec ~/bin/launcher.sh to launch Rofi with OneDark colors. 
