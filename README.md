   * [Introduction](#introduction)
   * [TMUX](#tmux)
      * [Plugins](#plugins)
      * [Keybindings](#keybindings)
   * [ZSH](#zsh)
   
```
████████▄   ▄██████▄      ███        ▄████████  ▄█   ▄█          ▄████████    ▄████████ 
███   ▀███ ███    ███ ▀█████████▄   ███    ███ ███  ███         ███    ███   ███    ███ 
███    ███ ███    ███    ▀███▀▀██   ███    █▀  ███▌ ███         ███    █▀    ███    █▀  
███    ███ ███    ███     ███   ▀  ▄███▄▄▄     ███▌ ███        ▄███▄▄▄       ███        
███    ███ ███    ███     ███     ▀▀███▀▀▀     ███▌ ███       ▀▀███▀▀▀     ▀███████████ 
███    ███ ███    ███     ███       ███        ███  ███         ███    █▄           ███ 
███   ▄███ ███    ███     ███       ███        ███  ███▌    ▄   ███    ███    ▄█    ███ 
████████▀   ▀██████▀     ▄████▀     ███        █▀   █████▄▄██   ██████████  ▄████████▀  
                                                    ▀                                   
```
## Introduction 

In this repository you will find my various dotfiles for your consumption. These days I find myself using various virtualization deployments that I needed to standarize my environment and make it reproduable with as little a headache as possible. I recently moved my workflow to only using a Tiling window manager such as Xmonad and bspWM with keybindings for launching and app navigation and I really dig the no titlebars look, it makes for a much cleaner experience. I am also a firm believer that your desktop should be personal and customized experience and I tend to invest some time into doing just that. The main branch will house the configs of the supporting cast of applications i use almost daily. My configs change to coincide with my current colorscheme and theme so this repo will be broken down with each unique setup having its own branch. As of now, that only consists of Xmonad and Bspwm setup.  I do not plan on evaluating any other Tiling window managers as I have these to a point that works beautifully for me and my workflow. Hopefully I have documented them enough for you to be able to clone the setup or utilize different aspects of each one. 



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
Other: 
<ul>
<li>laktak/extrakto (requires fzf)</li>
<li>thewtex/tmux-mem-cpu-load</li></ul>
	
I also use vim-tmux-pilot by sourcing pilot.tmux from my tmux config. This is installed via packer within nvim environment. 


### Keybindings

| Keys                           | Command          |
|--------------------------------|------------------|
| <kbd>Ctrl</kbd>+<kbd>A</kbd>   | send-prefox      |
| <kbd>Ctrl</kbd>+<kbd>Y</kbd>	 | sync panes 		|
| <kbd>x</kbd>			  		 | kill-pane 		|
| <kbd>X</kbd>			 		 | kill-window 		|
| <kbd>_</kbd>			  		 | split-window -v	|
| <kbd> \| </kbd>		 		 | splint-window -h |
| <kbd>z</kbd> 		 			 | resize-pane -Z   |
| <kbd>-</kbd> 		 			 | resize-pane -D 20 |
| <kbd>+</kbd> 		 			 | resize-pane -U 20 |
|  "<"                       	 | resize-pane -L 20 | 
|  <kbd> > </kbd> 		 		 | resize-pane -R 20 |
   


## ZSH 

I run zsh shell managed by oh-my-zsh for added functions via plugins. In all honesty I probably won't be running it much longer as the spaceship prompt is no longer depemdent on oh-my-zsh for functionality. The biggest culprit for my nerely 1 second start-up time was nvm and condainit. I have moved nvm to a lazy load and plan on doing the same for conda environments moving forward. the rc is fairly straight forward, custom functions for extracting archives and editing various confs, those are as follows:

conf function

```shell
conf() {
	case $1 in
		xmonad)		nvim ~/.xmonad/xmonad.hs ;;
		bspwm)		nvim ~/.config/bspwm/bspwmrc ;;
		sxhkd)		nvim ~/.config/sxhkd/sxhkdrc ;;
		polybar)	nvim ~/.config/polybar/config.ini ;;
		mpd)		nvim ~/.mpd/mpd.conf ;;
		mutt)		nvim ~/.muttrc ;;
		ncmpcpp)	nvim ~/.ncmpcpp/config ;;
		pacman)		svim /etc/pacman.conf ;;
		ranger)		nvim ~/.config/ranger/rc.conf ;;
		rifle)		nvim ~/.config/ranger/rifle.conf ;;
		tmux)		nvim ~/.tmux.conf ;;
		nvim)		nvim ~/.vimrc ;;
		xinit)		nvim ~/.xinitrc ;;
		xresources)	nvim ~/.Xresources && xrdb merge ~/.Xresources ;;
		zathura)	nvim ~/.config/zathura/zathurarc ;;
		theme2)		nvim ~/.config/gtk-2.0/gtkrc ;;
		theme3)		nvim ~/.config/gtk-3.0/gtk.css ;;
		gtk2)		nvim ~/.gtkrc-2.0 ;;
		gtk3)		nvim ~/.config/gtk-3.0/settings.ini ;;
		tint2)		nvim ~/.config/tint2/tint2rc ;;
		zsh)		nvim ~/.zshrc && source ~/.zshrc ;;
		hosts)		sudoedit /etc/hosts ;;
		vhosts)		sudoedit /etc/httpd/conf/extra/httpd-vhosts.conf ;;
		httpd)		sudoedit /etc/httpd/conf/httpd.conf ;;
		*)			echo "Unknown application: $1" ;;
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
