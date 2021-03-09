## Desktop environment 

This small change prompted an entire overhaul of my workflow to see if I could come up with an answer to, "Can I be more productive?". I waste a lot of time snapping windows taskwarrir

I settled on using a tiling window manager with key-bindings/scripts for various needs. The usual suspects here were i3gaps, awesome, qtile and xmonad. While I test drove each with varying success, I settled on an xmonad setup, mainly because it just worked for me. I'm using (modified) picom X compositor, dunst for notifcations and rofi for application switching and launching. Hotkeys for pop-out sidebar and dashboard views using Elkowar's Wacky Widget's and tint2 for dock and top panel with candy-icons theme.  

The base setup is [Axarva's dotfiles-2.0 repo](http://www.github.com/Axarva/dotfiles-2.0/). You can find my customizations in my [dotfiles](http://www.github.com/agonzal/dotfiles) github repo. I customized xmonad config to my liking, adding several keybindings and utlizing doFloat and doShift to set window properties for various apps like zoom and spotify.  



![xmonad-setup](/screenshots/xmonad-setup.png)

Current

- xmonad (DE)
- [picom](http://www.github.com/jonaburg/picom/) (X compositor)
- [tint2](http://www.github.com/Axarva/tint2-1/) (panels, dock and taskbar)
- eww (sidebar and dashboard)
- dunst (notifications)
- rofi (app switch/launch)
- tilix (tmux + powerline)
- zsh (oh-my-zsh + power10k theme)
- Thunar (file manager)
- firefox w/ vimium (vim keybindings)
- neovim/vim 
- FiraCode Nerd Font w/ ligatures and Glyph's. 

Code: 

I am always on the command line for the vast majority of my day/tasks. While I had been using vscodium I wanted to ditch it and go back to vim/nvim. I am familiar with the keybindings
and it just works for me. Coupled with an extensive plugin ecosystem you can achieve almost like for like replacement of vscode (CoC anyone?) not to mention for resource footprint is 
non-existant compared to the 400mb+for vscodium on a good-day. I am striving to move to mouse-free development environment. I am well on my way but I still have a ways to go before i can fully
ditch my mouse during coding sessions.

- Neovim/vim/Gvim 
  - Vundle plugin manager
  - NERDTree (tabs, devicons, syntax highlighting)
  - CoC (must-have)
  - Python/CSS/Javascript related plugins
  - FiraCode Nerd Font / Source Code Pro Nerd Font 
  - Obsidian markdown notes organizer 
  - Have not found a console based alternative with this extensive feature set. 

  
### Keybindings



|        Keybind         |                 Function                 |
| ---------------------- | ---------------------------------------- |
| `Win + Shift + Enter`  | Launch terminal (tilix)                  |
| `Win + Shift + C`   	 | Close window                             |
| `Win + O`              | Open rofi menu                           |
| `Win + Shift + Q`   	 | Open rofi powermenu                      |
| `Win + S`              | Open sidebar                             |
| `Win + Shift + S`   	 | Close sidebar                            |
| `Win + Control + S`    | Open rofi ssh menu                       |
| `Win + Control + R`    | Open calibre library                     |
| `Win + P`              | Open dashboard                           |
| `Win + Shift + P`	     | Close dashboard                          |
| `Win + B`              | Toggle the main bars                     |
| `Win + Z`              | Activate inhibitor (Stops autolocking)   |
| `Win + Shift + Z`	     | Dectivate inhibitor (Enables autolocking)|
| `Win + Ctrl + G`    	 | Toggle gaps (toggle to get screen space) |
| `Win + J`              | Navigate through windows                 |
| `Win + K`              | Navigate through windows                 |
| `Win + Space`          | Switch through layouts                   |
| `Win + T`              | Make a floating window tiled             |
| `Win + F`              | Launches Firefox                         |
| `Win + Left Click`     | doFloat                                  |
| `Win + Right Click`    | Resize window and doFloat                |


### Installation


You'll be required to make several changes to get eerything working just right. Unfrotunately this is not a clone and play, but we will get you 90% of the way there!

1.  If you want to see the weather
and a random quote in the dashboard view, you'll need to add new crontab entries `crontab -e`

```bash
0,30 * * * * /home/{Your-username}/.config/eww/scripts/getweather
@reboot /home/{Your-username}/.config/eww/scripts/getquotes
```

Note: You must get your own API key from openweathermap and your unique city ID and put them into eww/scripts/getweather for it to work!

2. If` $HOME/bin` is not in your PATH it must be moving forward. Add to bash/zsh whichever you use. 



3. Build [tint2](http://www.github.com/Axarva/tint2-1/) and mv tint2 dir to ~/.config/tint2
4. Build [picom](http://www.github.com/jonaburg/picom/) and copy picom.conf to ~/.config/picom/picom.conf
5. Copy thisrepoo/bin to ~/bin + make necessary modifications. 
6. Move thisrepo/eww to ~/.config/eww and modify scripts accordingly. I use systemctl. 
  a. Set your profile pic in eww.scss 
  b. change goodbye message in powermenu (grep -nR electr0n eww/scripts)
7. copy thisrepo/dunst to ~/.config/dunst
8. copy thisrepo/rofi to ~/.config/rofi 

I have been running this current setup since the new year and I am really satisfied with the outcome. I have even changed my dual monitor workflow as now I use them stacked with laptop below my 32" external monitor. Honestly I do not know why I have done side-by-side for all these years, this setup just feels good, cannot wait until I move to full dev w/o mouse!
