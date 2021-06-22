   * [Introduction](#introduction)
   * [Software](#Software)
   * [TMUX](#tmux)
      * [Plugins](#plugins)
      * [Keybindings](#keybindings)
   * [ZSH](#zsh)
   
```
             ______   _______ _______ _______ ___ ___     _______ _______ 
           |   _  \ |   _   |       |   _   |   |   |   |   _   |   _   |
           |.  |   \|.  |   |.|   | |.  1___|.  |.  |   |.  1___|   1___|
           |.  |    |.  |   `-|.  |-|.  __) |.  |.  |___|.  __)_|____   |
           |:  1    |:  1   | |:  | |:  |   |:  |:  1   |:  1   |:  1   |
           |::.. . /|::.. . | |::.| |::.|   |::.|::.. . |::.. . |::.. . |
           `------' `-------' `---' `---'   `---`-------`-------`-------'

```
## Introduction 

In this repository you will find my various dotfiles for your consumption. These days I find myself using various virtualization deployments that I needed to standarize my environment and make it reproduable with as little a headache as possible.(read: minimal effort). I recently moved my workflow to only using a Tiling window manager such as Xmonad and bspWM with keybindings to complete most tasks including app launching/navigation. I am believe that your desktop should be personal and a customized experience and I tend to invest some time into doing just that. As I stated I moved away from a traditional Desktop environment to a TWM (Tiling Window Manager). I have complete configurations for XMonad and BSPWM. 

Regardless of which tiling window manager I am using I try to mimic the environments as close as possible. You can find the configuration for BSPWM in .config/bspwm and XMONAD in .xmonad


## Software 

I have a lot of software that is "themable" which allows me to have a cohesive colorscheme system wide with the main apps I use. If I have forgotten to include to the config for an application listed here, please do not hesitate to contact me or setup an issue. 

| Usage           | Application               |
|-----------------|---------------------------|
| PDF/Read        | Zathura (OneDark)         |
| Kb/Notes        | Obsidian (OneDark)        |
| Editor          | Neovim / Neovide (GUI)    |
| Shell           | ZSH w/ spaceship prompt   |
| Termminal       | kitty (onedark colors)    |
| Files           | ranger / thunar (onedark) |
| 

## TMUX 

Locally I use kitty which has multiplexer functionality built right in but when I am logged into any server I have a standard TMUX configuration I use across the board. I use TPM to manage plugins and have custom keybindings for some things to make my life easier. Since I map caps to esc and/or ctrl I use ctrl+a (so I can hit caps+a) as my TMUX prefix. 

### Plugins 

tmux-plugins::
<ul>
	<li> tmux-sensible</li>
    <li> tmux-copycatM</li>
    <li> tmux-yank</li>
    <li> tmux-open</li>
</ul>
other:: 
<ul>
<li>laktak/extrakto (requires fzf)</li>
<li>thewtex/tmux-mem-cpu-load</li></ul>
	
I also use vim-tmux-pilot by sourcing pilot.tmux from my tmux config. This is installed via packer within nvim environment. 


### Keybindings

| Keys                           | Command          |
|--------------------------------|-------------------|
| <kbd>Ctrl</kbd>+<kbd>A</kbd>   | send-prefox       |
| <kbd>Ctrl</kbd>+<kbd>Y</kbd>	 | sync panes 	  	 |
| <kbd>x</kbd>			  		       | kill-pane 		     |
| <kbd>X</kbd>			 		         | kill-window 	   	 |
| <kbd>_</kbd>			  		       | split-window -v 	 |
| <kbd> \| </kbd>		 		         | splint-window -h  |
| <kbd>z</kbd> 		 			         | resize-pane -Z    |
| <kbd>-</kbd> 		 		        	 | resize-pane -D 20 |
| <kbd>+</kbd> 		 		        	 | resize-pane -U 20 |
| <kbd>  "<" </kbd>           	 | resize-pane -L 20 | 
| <kbd> ">" </kbd> 		 		       | resize-pane -R 20 |
|---------------------------------|------------------|


## ZSH 

I run zsh shell managed by oh-my-zsh for added functions via plugins. In all honesty I probably won't be running it much longer as the spaceship prompt is no longer depemdent on oh-my-zsh or node. The project made it a native zsh prompt via prompt -s. The biggest culprit for my nerely 2.2 second start-up time was nvm and conda init.
There is no need to try and load nvm, doctl, kubectl and the like at startup. Much more efficient to wrap these commands in a function that checks if you called the command and then provides 

conf function

```shell
conf() {
	case $1 in
		xmonad)		nvim ~/.xmonad/xmonad.hs ;;
		bspwm)		nvim ~/.config/bspwm/bspwmrc ;;
		sxhkd)		nvim ~/.config/sxhkd/sxhkdrc ;;
		polybar)	nvim ~/.config/polybar/config.ini ;;
		mpd)	  	nvim ~/.mpd/mpd.conf ;;
		mutt)	  	nvim ~/.muttrc ;;
		ncmpcpp)	nvim ~/.ncmpcpp/config ;;
		pacman)		svim /etc/pacman.conf ;;
		ranger)		nvim ~/.config/ranger/rc.conf ;;
		rifle)		nvim ~/.config/ranger/rifle.conf ;;
		tmux)	  	nvim ~/.tmux.conf ;;
		nvim)	  	nvim ~/.vimrc ;;
		xinit)		nvim ~/.xinitrc ;;
		xresources)	nvim ~/.Xresources && xrdb merge ~/.Xresources ;;
		zathura)	nvim ~/.config/zathura/zathurarc ;;
		theme2)		nvim ~/.config/gtk-2.0/gtkrc ;;
		theme3)		nvim ~/.config/gtk-3.0/gtk.css ;;
		gtk2)		  nvim ~/.gtkrc-2.0 ;;
		gtk3)		  nvim ~/.config/gtk-3.0/settings.ini ;;
		tint2)		nvim ~/.config/tint2/tint2rc ;;
		zsh)		  nvim ~/.zshrc && source ~/.zshrc ;;
		hosts)		sudoedit /etc/hosts ;;
		vhosts)		sudoedit /etc/httpd/conf/extra/httpd-vhosts.conf ;;
		httpd)		sudoedit /etc/httpd/conf/httpd.conf ;;
		*)			  echo "Unknown application: $1" ;;
	esac
}
```

extract function

```shell
function extract {
 if [ -z "$1" ]; then
    # display usage if no parameters given
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
 else
    for n in "$@"
    do
      if [ -f "$n" ] ; then
          case "${n%,}" in
            *.cbt|*.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
                         tar xvf "$n"       ;;
            *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;
            *.cbr|*.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;
            *.cbz|*.epub|*.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;;
            *.7z|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
                         7z x ./"$n"        ;;
            *.xz)        unxz ./"$n"        ;;
            *.exe)       cabextract ./"$n"  ;;
            *.cpio)      cpio -id < ./"$n"  ;;
            *.cba|*.ace)      unace x ./"$n"      ;;
            *)
                         echo "extract: '$n' - unknown archive method"
                         return 1
                         ;;
          esac
      else
          echo "'$n' - file does not exist"
          return 1
      fi
    done
fi
}
```

The other functions deal with lf and ranger which are both terminal based file managers (e.g. file icons for LF, able to hit q to quit ranger)
