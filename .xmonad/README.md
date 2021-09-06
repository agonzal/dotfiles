```
     ___ ___  ___ ___ _______ ______  _______ ______   
    (   Y   )|   Y   |   _   |   _  \|   _   |   _  \  
     \  1  / |.      |.  |   |.  |   |.  1   |.  |   \ 
     /  _  \ |. \_/  |.  |   |.  |   |.  _   |.  |    \
    /:  |   \|:  |   |:  1   |:  |   |:  |   |:  1    /
   (::. |:.  |::.|:. |::.. . |::.|   |::.|:. |::.. . / 
    `--- ---'`--- ---`-------`--- ---`--- ---`------'  
                   -=-=-=-=-=-=-=-=-=-=-=-=:: electr0n 
```

## Preview 

![current-xmonad-setup](current-xmonad.png)



## Config overview 

 - Imports:
   - XMonad / Data / System / Control - Ln: 24 --> 33 
   - Polybar imports - Ln: 37 --> 39 
   - XMonad Utils - Ln: 41 --> 45
   - XMonad Graphics - Ln: 48 
   - XMonad Hooks - Ln: 52 --> 58
   - Custom Modules - Ln: 61 --> 62
   - XMonad Actions - Ln: 65 --> 73 
   - XMonad Layouts - Ln: 76 --> 93 
 - myBorderWidth = 2 (colors match OneDark) Ln: 113
 - myModMask = mod4Mask (Super aka Windowsi key or Ubuntu logo for me) Ln: 114 
 - 9 workspaces defined using their HEX code from nerd-fonts. Ln: 117 
   - Term | Chrome | Vim | Reading | Slack | Visualizations | DataSci | Droplets | Analysis 
 - myTabConfig def - Ln: 120 --> 130 
 - standardTheme def - Ln: 134 --> 145 (for SideDecoration module)
 - myScratchPads - Ln: 149 --> 154 
 - Custom floating functions Ln: 164 --> 182 
   - toggleFloat (float + center a tiled window, tile a floating window) 
   - centerFloat 
      - float and center a tiled window, retaining size while it was tiled. 
   - centerFloat'
      - float and center a tiled window 1/4 of screen. 
   - customFloat 
      - float and resize window to my standard size, half of screen. 
 - addNETSupported / addEWMHFullscreen functions Ln: 183 --> 197 
 - myKeys (mod4Mask as modm) Ln: 202 --> 371
 - myMouseBindings - Ln: 394 --> 404 
 - myLayout - Ln: 411 --> 430
 - myManageHook - Ln: 436 --> 463 
 - myEventHook - Ln: 474 
 - mkDbusClient - Ln: 494 --> 500 
 - dbusOutput - Ln: 503 --> 510 
 - myLogHook - Ln: 513
 - main - Ln: 551 --> 584

### ScartchPads 

I made heavy use of NamedScratchpad before my discovery of [tdrop](https://github.com/noctuid/tdrop) by noctuid. 

- ScratchPads for :
   - kitty (with --class scratchpad)
   - ncmpcpp 
   - bpytop (replaced by tdrop)
   - thunar (replaced by tdrop)
 
 ScartchPads utilize the ALT + {t,m,b,f} depending on which one you want to summon. ScartchPads utilize myPositiion which basically is a centerFloat via 
 W.RationalRect (1/3) (1/3) (1/3) (1/3). The NSP are still active in this configuration but are being phased out by tdrop. As it utilizes xdotool to essentially 
 hide the window in question. Using the same keybinding it will reappear in the designated area. The tdrop script essentially lets you turn any app into a "dropdown". 
 I initially started using it because Thunar would not work as an XMonad scratchpad and sometimes you just don't want to run a file-manager in the console. 

### Layouts 

 My configuration utilizes the following order for layouts:

 - centered (ThreeColMid)
 - tabs 
 - tiled 
 - Mirror tiled

     where 

     tiled utilizes ResizableTall from XMonad.Layout.ResizableTile module. Ratio is 2/3 + delta 1/100. 

     centered utilizes ThreeColMid with 1 nmaster 3/100 delta and 1/2 ratio. 

     tabs sets tabbedBottom shrinkText myTabConfig which is defined on Ln 91. 

The relevant layout declaration looks like: 

          ```myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ minimize $ defaultFancyBorders (avoidStruts ( centered ||| tiled ||| (Mirror tiled ||| tabs))) ```

  As you can see, I am using mkToggle from XMonad.Layout.MultiToggle. This allows for keybindings to be defined to toggle NBFULL and NOBORDERS. then the defined layouts
  are wrapped in avoidstruts from XMonad.Hooks.ManageDocks and of course XMonad.Layout.Gaps which are L 10 R 10 U 10 D 30. I then wrap avoidStruts in a custom FancyBorders module
  that provides double borders ala herbstluftwm. While not currently being utilized you can still find it in the repo, SideDecorations. This provides the ability to only
  set a window border on a specific side of each window (on specific or all layouts if you choose). Of course all of this is wrapped up by Minimize layout module (and Hook). I 
  have defined a keybinding to minimize a window but not to maximize said window as I could not get that to work even though I was following the instructions and utilizing a working
  example from another configuration. No biggie as I use polywins.sh from my polybar config to bring the window I minimized back from hiding! 
  

### ManageHook

  myManageHook has definitions for fullscreen support, manageDocks, manageSpawns and namedScatchPadManageHook. I also heavily utilize className / resource / title for 
  automatic window rules and placement. Lines 341 to 371. I need to rework this section to use variables that way I can have 3 to 5 defintions for each variable group, will 
  result in much cleaner config. 

### StartupHook

This section invokes ~/bin/autolock.sh for xautolock. This is also where we set WMname to LG3D as a work-around to get CobaltStrike
and other Java based apps to run properly inside our WM-only environment. Most of these apps expect to be running under a full DE(Desktop Environment)
under Linux and that is not the case for my setup. With that said, I use my startup hook and spawnOnce to launch at start the applets I
use as well as set my cursor pointer, restore the background, run polkit agent and I use setxkbmap to map CAPS as an <kbd>esc</kbd> when
used alone or as a <kbd>control</kbd> key when used with another key. Another little cheat I use is xcape to make Super_L map to Super + o 
(invoke rofi) when pressed alone. 

  - numlockx 
  - nm-applet 
  - volumeicon 
  - gnome-polkit 
  - setxkbmap 
  - xcape 
  - picom 
  - nitrogen --restore  
  - xsetroot for mouse pointer 

You can find the relevant section in the configuration from lines 530 --> 547. 

### main 

 I do have dbus awareness here but I am not currently using this as it was simpler to integrate with polybar just using the ewmh module without xmonad-log, dbus, etc... The 
 relevant code is (Line 494 --> 510):

 ```haskell 
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
 ```

 It is relevant to see these functions as they are both referenced from main. Line 551 --> 552 and 557 --> 558 respectively.

 ```haskell

main :: IO ()
main = mkDbusClient>>= main' 
 ```
and 

```haskell 
main' :: D.Client -> IO ()
main' dbus = xmonad $ docks $ ewmh defaults 

```
 I call xmonad via 'xmonad $ docks $ ewmh defaults '

 You can find defaults defined at line 422: 


StartupHook = myStartupHook >> addEWMHFullscreen calls this for actual fullscreen support as using XMonad.Layout.Fullscreen would make certain apps fullscreen WITHIN the tiled dimensions (I am looking at you chrome)

```haskell 
addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]
```

HELP => Press Super + Shift + Front Slash (aka ?) to get an xmessage with default XMonad keybindings. Does not currently include keybindings for individual apps that i have specified but
that can all be solved by invoking Super + O to exec ~/bin/launcher.sh to launch Rofi with OneDark colors. 
