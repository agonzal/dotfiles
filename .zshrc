#    _______ _______ ___ ___ _______ _______ 
#   |   _   |   _   |   Y   |   _   |   _   |
#   |___|   |   1___|.  1   |.  R   |.  1___|
#    /  ___/|____   |.  _   |.  _   |.  |___ 
#   |:  1  \|:  1   |:  |   |:  |   |:  1   |
#   |::.. . |::.. . |::.|:. |::.|:. |::.. . |
#   `-------`-------`--- ---`--- ---`-------'
#################################################
##################### Albert "electr0n" Gonzalez 

# ZSH options 

setopt share_history 
setopt hist_ignore_dups 
setopt auto_cd 
setopt nobeep 
setopt hist_find_no_dups 


# Path to your oh-my-zsh installation.
export ZSH="/home/electr0n/.oh-my-zsh"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
ZSH_THEME=agonzal
plugins=( httpie tmux command-not-found fzf-tab zsh-autosuggestions zsh-completions zsh-syntax-highlighting history-substring-search z ripgrep fzf history zsh_reload systemd ssh-agent)

#For z
[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh


source $ZSH/oh-my-zsh.sh

export QT_QPA_PLATFORMTHEME=qt5ct
export PATH=$PATH:/home/electr0n/.local/bin:~/.nvm/versions/node:/home/electr0n/scripts:/home/electr0n/bin:~/.cargo/bin:~/.local/share/gem/ruby/3.0.0/bin:/home/electr0n/.local/share/gem/ruby/3.0.0/bin:~/.config/polybar/scripts:~/.tranalyzer2-0.8.10/scripts:~/.local/share/nvim/lspinstall/bash/node_modules/.bin
export less=" -R "
export LESSOPEN='| ~/bin/src-hilite-lesspipe.sh %s'
export BAT_THEME="TwoDark"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export PF_INFO="ascii title os kernel wm pkgs cpu memory shell term palette"
export LANG=en_US.UTF-8
export FZF_BASE=/usr/bin/fzf
export FZF_DEFAULT_OPTS='
  --color fg:#6f737b,bg:#21252d
  --color bg+:#adc896,fg+:#282c34,hl:#abb2bf,hl+:#1e222a,gutter:#282c34
  --color pointer:#adc896,info:#abb2bf,border:#565c64
  --border'


# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='nvim'
 fi

# Intelligently load completion for kitty  
if [ ${TERM} = "kitty" ]; then 
  kitty + complete setup zsh | source /dev/stdin 
fi

# directories for zsh autocompletions fzf + kitty etc...
fpath=(~/.zsh.d/ $fpath ~/.zfunctions)


# NVM 
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


# Check if 'kubectl' is a command in $PATH
if [ $commands[kubectl] ]; then

  # Placeholder 'kubectl' shell function:
  # Will only be executed on the first call to 'kubectl'
  kubectl() {

    # Remove this function, subsequent calls will execute 'kubectl' directly
    unfunction "$0"

    # Load auto-completion
    source <(kubectl completion zsh)

    # Execute 'kubectl' binary
    $0 "$@"
  }
fi

if [ $commands[navi] ]; then 
  navi() {
    unfunction "$0"

    source <(navi widget zsh)

    $0 "$@" # run navi, in our case my alias using fzf 
  }
fi 

if [ $commands[doctl] ]; then 

  doctl() {
    unfunction "$0"

    source <(doctl completion zsh)

    $0 "$@"
  }
fi 

#Add empty space between prompts
precmd() { print "" }

# hide division sign on incomplete line
PROMPT_EOL_MARK=''


export TERM_PROGRAM=kitty
export OPENER=rifle
#printf "\033]0; $(pwd | sed "s|$HOME|~|") - lf\007" > /dev/tty


# ZSH_HIGHLIGHT gray out command line arguments 
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets regexp)
typeset -A ZSH_HIGHLIGHT_REGEXP
ZSH_HIGHLIGHT_REGEXP+=(' -{1,2}[a-zA-Z0-9_-]*' fg=008)



if [ -f "/home/electr0n/.tranalyzer2-0.8.10/scripts/t2_aliases" ]; then
    . "/home/electr0n/.tranalyzer2-0.8.10/scripts/t2_aliases"
fi
