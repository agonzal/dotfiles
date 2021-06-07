#    _______ _______ ___ ___ _______ _______ 
#   |   _   |   _   |   Y   |   _   |   _   |
#   |___|   |   1___|.  1   |.  R   |.  1___|
#    /  ___/|____   |.  _   |.  _   |.  |___ 
#   |:  1  \|:  1   |:  |   |:  |   |:  1   |
#   |::.. . |::.. . |::.|:. |::.|:. |::.. . |
#   `-------`-------`--- ---`--- ---`-------'


zmodload zsh/zprof 

# Path to your oh-my-zsh installation.
export ZSH="/home/electr0n/.oh-my-zsh"

HYPHEN_INSENSITIVE="true"

COMPLETION_WAITING_DOTS="true"

plugins=(tmux fzf-tab zsh-autosuggestions zsh-completions zsh-syntax-highlighting history-substring-search thefuck z ripgrep fzf fd command-not-found history zsh_reload pass systemd npm )

#For z
[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh

export FZF_BASE=/usr/bin/fzf

source $ZSH/oh-my-zsh.sh

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export BAT_THEME="TwoDark"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export LANG=en_US.UTF-8
export FZF_DEFAULT_OPTS='
  --color fg:#6f737b,bg:#21252d
  --color bg+:#adc896,fg+:#282c34,hl:#abb2bf,hl+:#1e222a,gutter:#282c34
  --color pointer:#adc896,info:#abb2bf,border:#565c64
  --border'

 #eval "$(oh-my-posh --init --shell zsh --config ~/.poshthemes/spaceship.omp.json)"

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nvim'
 else
   export EDITOR='nvim'
 fi



alias nv=nvim 
alias vi=vim 
alias weather="curl 'wttr.in/?T'"
alias svim="sudoedit"
alias cht='f(){ curl -s "cheat.sh/$(echo -n "$*"|jq -sRr @uri)";};f'
alias rcd=". ranger"
alias gd="googler -n 4"
alias mdl="youtube-dl --add-metadata -xic -f bestaudio/best -o '~/Music/%(title)s.%(ext)s' --audio-format mp3 --embed-thumbnail"
alias tncmpcpp='tmux new-session "tmux source-file ~/.ncmpcpp/tmux_session"'
alias clip="xclip -sel clip"
alias logout="kill -9 -1"
alias ttcmd="echo '' | fzf -q '$*' --prompt '│ ' --pointer '― ' --preview-window=up:99% --preview='eval {q}'"
alias fet="info='n user os sh wm kern pkgs mem term col' separator=' | ' accent='2' fet.sh"
alias fm="setsid -f thunar"
alias vim="nvim"
alias c="bc -l"
alias btop="bpytop"
alias yi="yay -Slq | fzf -m --preview 'yay -Si {1}' | xargs -ro yay -S"
alias cat="bat"
alias pfetch="curl -s https://raw.githubusercontent.com/dylanaraps/pfetch/master/pfetch | sh"
alias navi="navi --fzf-overrides '--color fg:7,bg:-1,hl:6,fg+:6,bg+:-1,hl+:6,info:2,prompt:1,spinner:5,pointer:5,marker:3,header:8'"
alias xmr="xmonad --restart"
alias xmc="xmonad --recompile"
alias topmem='ps -e -o pid,cmd,%cpu,%mem --sort=-%mem | head -n 6'
alias hunt='ssh root0n@hunt.revrse.sh'
alias icat="kitty +kitten icat"
alias ll='colorls --dark -h -lA --sd --gs --group-directories-first'
alias ls='colorls --dark -h --gs --group-directories-first'


### colorls gem 
source $(dirname $(gem which colorls))/tab_complete.sh

#completion
autoload -Uz compinit && compinit
# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

#for thefuck
eval $(thefuck --alias)

# directories for zsh autocompletions fzf + kitty etc...
fpath=(~/.zsh.d/ $fpath ~/.zfunctions)



#     ___                  __   __                   
#   .'  _.--.--.-----.----|  |_|__.-----.-----.-----.
#   |   _|  |  |     |  __|   _|  |  _  |     |__ --|
#   |__| |_____|__|__|____|____|__|_____|__|__|_____|


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

### Speed up shell loading by only loading nvm when needed:
#

declare -a NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)

NODE_GLOBALS+=("node")
NODE_GLOBALS+=("nvm")

load_nvm () {
    export NVM_DIR=~/.nvm
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
}

for cmd in "${NODE_GLOBALS[@]}"; do
    eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_nvm; ${cmd} \$@ }"
done

# disable zsh-autocomplete autocorrections
#zstyle ':autocomplete:*' magic off

# Pfetch configuration
export PF_INFO="ascii title os kernel wm pkgs cpu memory shell term palette"
# for pfetch; term doesn't show properly in tmux

# pidswallow fix
[ -n "$DISPLAY" ]  && command -v xdo >/dev/null 2>&1 && xdo id > /tmp/term-wid-"$$"
trap "( rm -f /tmp/term-wid-"$$" )" EXIT HUP

# navi doctl kubectl auto completion 
source <(navi widget zsh)
source <(doctl completion zsh)
source <(kubectl completion zsh)


#Add empty space between prompts
#precmd() { print "" }

# hide division sign on incomplete line
PROMPT_EOL_MARK=''


export TERMINAL=kitty
export TERM_PROGRAM=kitty
export LF_ICONS="\
tw=:\
st=:\
ow=:\
dt=:\
di=:\
fi=:\
*.7z=:\
*.a=:\
*.ai=:\
*.apk=:\
*.asm=:\
*.asp=:\
*.aup=:\
*.avi=:\
*.awk=:\
*.bash=:\
*.bat=:\
*.bmp=:\
*.bz2=:\
*.c=:\
*.c++=:\
*.cab=:\
*.cbr=:\
*.cbz=:\
*.cc=:\
*.class=:\
*.clj=:\
*.cljc=:\
*.cljs=:\
*.cmake=:\
*.coffee=:\
*.conf=:\
*.cp=:\
*.cpio=:\
*.cpp=:\
*.cs=:\
*.csh=:\
*.css=:\
*.cue=:\
*.cvs=:\
*.cxx=:\
*.d=:\
*.dart=:\
*.db=:\
*.deb=:\
*.diff=:\
*.dll=:\
*.doc=:\
*.docx=:\
*.dump=:\
*.edn=:\
*.eex=:\
*.efi=:\
*.ejs=:\
*.elf=:\
*.elm=:\
*.epub=:\
*.erl=:\
*.ex=:\
*.exe=:\
*.exs=:\
*.f#=:\
*.fifo=|:\
*.fish=:\
*.flac=:\
*.flv=:\
*.fs=:\
*.fsi=:\
*.fsscript=:\
*.fsx=:\
*.gem=:\
*.gemspec=:\
*.gif=:\
*.go=:\
*.gz=:\
*.gzip=:\
*.h=:\
*.haml=:\
*.hbs=:\
*.hh=:\
*.hpp=:\
*.hrl=:\
*.hs=:\
*.htaccess=:\
*.htm=:\
*.html=:\
*.htpasswd=:\
*.hxx=:\
*.ico=:\
*.img=:\
*.ini=:\
*.iso=:\
*.jar=:\
*.java=:\
*.jl=:\
*.jpeg=:\
*.jpg=:\
*.js=:\
*.json=:\
*.jsx=:\
*.key=:\
*.ksh=:\
*.leex=:\
*.less=:\
*.lha=:\
*.lhs=:\
*.log=:\
*.lua=:\
*.lzh=:\
*.lzma=:\
*.m4a=:\
*.m4v=:\
*.markdown=:\
*.md=:\
*.mdx=:\
*.mjs=:\
*.mkv=:\
*.ml=λ:\
*.mli=λ:\
*.mov=:\
*.mp3=:\
*.mp4=:\
*.mpeg=:\
*.mpg=:\
*.msi=:\
*.mustache=:\
*.nix=:\
*.o=:\
*.ogg=:\
*.pdf=:\
*.php=:\
*.pl=:\
*.pm=:\
*.png=:\
*.pp=:\
*.ppt=:\
*.pptx=:\
*.ps1=:\
*.psb=:\
*.psd=:\
*.pub=:\
*.py=:\
*.pyc=:\
*.pyd=:\
*.pyo=:\
*.r=ﳒ:\
*.rake=:\
*.rar=:\
*.rb=:\
*.rc=:\
*.rlib=:\
*.rmd=:\
*.rom=:\
*.rpm=:\
*.rproj=鉶:\
*.rs=:\
*.rss=:\
*.rtf=:\
*.s=:\
*.sass=:\
*.scala=:\
*.scss=:\
*.sh=:\
*.slim=:\
*.sln=:\
*.so=:\
*.sql=:\
*.styl=:\
*.suo=:\
*.swift=:\
*.t=:\
*.tar=:\
*.tex=ﭨ:\
*.tgz=:\
*.toml=:\
*.ts=:\
*.tsx=:\
*.twig=:\
*.vim=:\
*.vimrc=:\
*.vue=﵂:\
*.wav=:\
*.webm=:\
*.webmanifest=:\
*.webp=:\
*.xbps=:\
*.xcplayground=:\
*.xhtml=:\
*.xls=:\
*.xlsx=:\
*.xml=:\
*.xul=:\
*.xz=:\
*.yaml=:\
*.yml=:\
*.zip=:\
*.zsh=:\ "


export OPENER=rifle
printf "\033]0; $(pwd | sed "s|$HOME|~|") - lf\007" > /dev/tty


export PATH=$PATH:/home/electr0n/.linuxbrew/bin:~/.nvm/versions/node:/home/electr0n/scripts:/home/electr0n/bin:~/.cargo/bin:~/.local/share/gem/ruby/3.0.0/bin:/home/electr0n/.local/share/gem/ruby/3.0.0/bin:~/.config/polybar/scripts

#export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!

__conda_setup="$('/home/electr0n/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/electr0n/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/electr0n/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/electr0n/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


## Trananalyzer

export T2HOME=/home/electr0n/Downloads/tranalyzer2-0.8.9


if [ -f "$T2HOME/scripts/t2_aliases" ]; then
    . $T2HOME/scripts/t2_aliases             # Note the leading `.'
fi

# eval "$(oh-my-posh --init --shell zsh --config ~/.poshthemes/darkblood.omp.json)"


export LESSOPEN='| ~/bin/src-hilite-lesspipe.sh %s'
export less=' -R '
sh $HOME/logo | lolcat 

  # Set Spaceship ZSH as a prompt
  autoload -U promptinit; promptinit
  prompt spaceship

  zprof 
