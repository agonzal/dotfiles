function proxy_on() {
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"

    if (( $# > 0 )); then
        valid=$(echo $@ | sed -n 's/\([0-9]\{1,3\}.\?\)\{4\}:\([0-9]\+\)/&/p')
        if [[ $valid != $@ ]]; then
            >&2 echo "Invalid address"
            return 1
        fi
        local proxy=$1
        export http_proxy="$proxy" \
               https_proxy=$proxy \
               ftp_proxy=$proxy \
               rsync_proxy=$proxy
        echo "Proxy environment variable set."
        return 0
    fi

    echo -n "username: "; read username
    if [[ $username != "" ]]; then
        echo -n "password: "
        read -es password
        local pre="$username:$password@"
    fi

    echo -n "server: "; read server
    echo -n "port: "; read port
    local proxy=$pre$server:$port
    export http_proxy="$proxy" \
           https_proxy=$proxy \
           ftp_proxy=$proxy \
           rsync_proxy=$proxy \
           HTTP_PROXY=$proxy \
           HTTPS_PROXY=$proxy \
           FTP_PROXY=$proxy \
           RSYNC_PROXY=$proxy
}

function proxy_off(){
    unset http_proxy https_proxy ftp_proxy rsync_proxy \
          HTTP_PROXY HTTPS_PROXY FTP_PROXY RSYNC_PROXY
    echo -e "Proxy environment variable removed."
}


genplaylist() {
	cd ~/tunes
	find . -name '*.mp3' -o -name '*.flac'|sed -e 's%^./%%g' > ~/.mpd/playlists/all.m3u
	mpc clear
	mpc load all.m3u
	mpc update
}

pk() {
  pid=$(ps -ef | sed 1d | fzf --reverse -m --ansi --color prompt:166,border:46 --height 40%  --border=sharp --prompt="➤  " --pointer="➤ " --marker="➤ " | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

select_file() {
    given_file="$1"
    #fd --type file --follow --hidden --exclude .git | fzf --query="$given_file"
    fzf --query="$given_file"
}

# extract function
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
            *.lzma)             unlzma ./"$n"      ;;
            *.bz2)              bunzip2 ./"$n"     ;;
            *.cbr|*.rar)        unrar x -ad ./"$n" ;;
            *.gz)               gunzip ./"$n"      ;;
            *.cbz|*.epub|*.zip) unzip ./"$n"       ;;
            *.z)                uncompress ./"$n"  ;;
            *.7z|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
                                7z x ./"$n"        ;;
            *.xz)               unxz ./"$n"        ;;
            *.exe)              cabextract ./"$n"  ;;
            *.cpio)             cpio -id < ./"$n"  ;;
            *.cba|*.ace)        unace x ./"$n"     ;;
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

## Quit ranger with Q. 

function ranger {
    local IFS=$'\t\n'
    local tempfile="$(mktemp -t tmp.XXXXXX)"
    local ranger_cmd=(
        command
        ranger
        --cmd="map Q chain shell echo %d > "$tempfile"; quitall"
    )
    
    ${ranger_cmd[@]} "$@"
    if [[ -f "$tempfile" ]] && [[ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]]; then
        cd -- "$(cat "$tempfile")" || return
    fi
    command rm -f -- "$tempfile" 2>/dev/null
}


conf() {
	case $1 in
		xmonad)		    nvim ~/.xmonad/xmonad.hs ;;
		bspwm)		    nvim ~/.config/bspwm/bspwmrc ;;
    bpytop)       nvim ~/.config/bpytop/themes/onedark.theme ;;
    bsp-autostart)    nvim ~/.config/bspwm/autostart.sh ;;
		sxhkd)		    nvim ~/.config/sxhkd/sxhkdrc ;;
		polybar)	    nvim ~/.config/polybar/config.ini ;;
    polywins)     nvim ~/.config/polybar/scripts/polywins.sh ;;
		mpd)		      nvim ~/.mpd/mpd.conf ;;
    kitty)        nvim ~/.config/kitty/kitty.conf ;;
		mutt)		      nvim ~/.muttrc ;;
    neofetch)     nvim ~/.config/neofetch/config.conf ;; 
    aerc)         nvim ~/.config/aerc/aerc.conf ;;
		ncmpcpp)	    nvim ~/.ncmpcpp/config ;;
		pacman)		    svim /etc/pacman.conf ;;
    picom)        nvim ~/.config/picom/picom.conf ;;
		ranger)		    nvim ~/.config/ranger/rc.conf ;;
		rifle)		    nvim ~/.config/ranger/rifle.conf ;;
		tmux)		      nvim ~/.tmux.conf ;;
		nvim)		      nvim ~/.vimrc ;;
		xinit)		    nvim ~/.xinitrc ;;
		xres)	        nvim ~/.Xresources && xrdb merge ~/.Xresources ;;
		zathura)	    nvim ~/.config/zathura/zathurarc ;;
		theme2)		    nvim ~/.config/gtk-2.0/gtkrc ;;
		theme3)		    nvim ~/.config/gtk-3.0/gtk.css ;;
		gtk2)		      nvim ~/.gtkrc-2.0 ;;
		gtk3)		      nvim ~/.config/gtk-3.0/settings.ini ;;
		tint2)		    nvim ~/.config/tint2/tint2rc ;;
		zsh)		      nvim ~/.zshrc && source ~/.zshrc ;;
		hosts)		    sxim /etc/hosts ;;
		vhosts)		    svim /etc/httpd/conf/extra/httpd-vhosts.conf ;;
    http)         svim /etc/httpd/conf/httpd.conf ;;
		*)			      echo "Unknown application: $1" ;;
	esac
}

lf () {
    tmp="$(mktemp)"
    /usr/bin/lf --last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}


any() {
	if [[ -z "$1" ]] ; then
		echo "any - grep for process(es) by keyword" >&2
		echo "Usage: any <keyword>" >&2 ; return 1
	else
		local STRING=$1
		local LENGTH=$(expr length $STRING)
		local FIRSCHAR=$(echo $(expr substr $STRING 1 1))
		local REST=$(echo $(expr substr $STRING 2 $LENGTH))
		ps xauwww| grep "[$FIRSCHAR]$REST"
	fi
}

