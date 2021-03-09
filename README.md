## Dotfiles repo

I use stow to manage my dotfiles and keep them insync with git(hub). 
This repo is for all my dotfiles, from nvim/vim to irssi but also represent my current xmonad setup based on Axarva dotfiles-2.0
repo. The base config is his, I just highly modified it to my liking. I haven't 
quite figured out eww to display correctly at 2880x1620 so the sidebar and centerlayout
are slightly off. Here is what it looks like now:

<img src="/screenshots/xmonad-setup.jpg">

What you'll need to reproduce is: 

- xmonad
- eww (binary included in axarva/dotfiles-2.0)
- firefox
- tint2 (Axarva fork of tint2)
- rofi 
- picom [jonaburg's picom fork](https://github.com/jonaburg/picom/)


You can check out Axarva dotfiles-2.0 repo for full installation instructions. 
The main reason for my move to xmonad is for keybindings for everything! The basic one 
for this setup are:


|        Keybind         |                 Function                 |
| ---------------------- | ---------------------------------------- |
| `Win + Shift + Enter`  | Launch terminal (tilix)                  |
| `Win + Shift + C`      | Close window                             |
| `Win + O`              | Open rofi menu                           |
| `Win + Shift + Q`      | Open rofi powermenu                      |
| `Win + S`              | Open sidebar                             |
| `Win + Shift + S`      | Close sidebar                            |
| `Win + Control + S`    | Open rofi ssh menu                       |
| `Win + Control + R`    | Open calibre library                     |
| `Win + P`              | Open dashboard                           |
| `Win + Shift + P`      | Close dashboard                          |
| `Win + B`              | Toggle the main bars                     |
| `Win + Z`              | Activate inhibitor (Stops autolocking)   |
| `Win + Shift + Z`      | Dectivate inhibitor (Enables autolocking)|
| `Win + Ctrl + G`       | Toggle gaps (toggle to get screen space) |
| `Win + J`              | Navigate through windows                 |
| `Win + K`              | Navigate through windows                 |
| `Win + Space`          | Switch through layouts                   |
| `Win + T`              | Make a floating window tiled             |
| `Win + F`              | Launches Firefox                         |
| `Win + Left Click`     | doFloat                                  |
| `Win + Right Click`    | Resize window and doFloat                |


