export QT_QPA_PLATFORMTHEME=qt5ct
export PATH=$PATH:/home/electr0n/.local/bin:~/.nvm/versions/node:/home/electr0n/scripts:/home/electr0n/bin:~/.cargo/bin:~/.local/share/gem/ruby/3.0.0/bin:/home/electr0n/.local/share/gem/ruby/3.0.0/bin:~/.config/polybar/scripts:~/.tranalyzer2-0.8.10/scripts:~/.local/share/nvim/lspinstall/bash/node_modules/.bin:/opt/Brim/resources/app.asar.unpacked/zdeps:~/.dotnet/tools:~/.gem/ruby/3.0. 
export less=" -R "
export LESSOPEN='| ~/bin/src-hilite-lesspipe.sh %s'
export BAT_THEME="OneHalfDark"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export PF_INFO="ascii title os kernel wm pkgs cpu memory shell term palette"
export LANG=en_US.UTF-8
export FZF_BASE=/usr/bin/fzf
export FZF_DEFAULT_OPTS='
  --color fg:#6f737b,bg:#21252d
  --color bg+:#adc896,fg+:#282c34,hl:#abb2bf,hl+:#1e222a,gutter:#282c34
  --color pointer:#adc896,info:#abb2bf,border:#565c64
  --border'

export OPENER=rifle
printf "\033]0; $(pwd | sed "s|$HOME|~|") - lf\007" > /dev/tty


# ZSH_HIGHLIGHT gray out command line arguments 
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets regexp)
typeset -A ZSH_HIGHLIGHT_REGEXP
ZSH_HIGHLIGHT_REGEXP+=(' -{1,2}[a-zA-Z0-9_-]*' fg=008)
