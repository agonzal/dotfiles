#    _______ _______ ___ ___ _______ _______ 
#   |   _   |   _   |   Y   |   _   |   _   |
#   |___|   |   1___|.  1   |.  R   |.  1___|
#    /  ___/|____   |.  _   |.  _   |.  |___ 
#   |:  1  \|:  1   |:  |   |:  |   |:  1   |
#   |::.. . |::.. . |::.|:. |::.|:. |::.. . |
#   `-------`-------`--- ---`--- ---`-------'



# Path to your oh-my-zsh installation.
export ZSH="/home/electr0n/.oh-my-zsh"
#source $ZSH/custom/agonzal.zsh-theme
ZSH_THEME='agonzal'

### colorls gem 
#source $(dirname $(gem which colorls))/tab_complete.sh

HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"

plugins=(tmux command-not-found fzf-tab zsh-autosuggestions zsh-completions zsh-syntax-highlighting history-substring-search z ripgrep fzf history zsh_reload pass systemd ssh-agent)

#For z
[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh


source $ZSH/oh-my-zsh.sh
#source $ZSH/custom/aliases.zsh 
ZSH_THEME='agonzal'

export PATH=$PATH:/home/electr0n/.linuxbrew/bin:~/.nvm/versions/node:/home/electr0n/scripts:/home/electr0n/bin:~/.cargo/bin:~/.local/share/gem/ruby/3.0.0/bin:/home/electr0n/.local/share/gem/ruby/3.0.0/bin:~/.config/polybar/scripts:~/tranalyzer2-0.8.9/scripts
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

## Trananalyzer

export T2HOME=/home/electr0n/tranalyzer2-0.8.9
if [ -f "$T2HOME/scripts/t2_aliases" ]; then
    . $T2HOME/scripts/t2_aliases             # Note the leading `.'
fi


 #eval "$(oh-my-posh --init --shell zsh --config ~/.poshthemes/spaceship.omp.json)"

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


# ZSH_HIGHLIGHT gray out command line arguments 
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets regexp)
typeset -A ZSH_HIGHLIGHT_REGEXP
ZSH_HIGHLIGHT_REGEXP+=(' -{1,2}[a-zA-Z0-9_-]*' fg=008)


