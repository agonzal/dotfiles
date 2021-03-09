# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH:/opt/metasploit-framework/bin:"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
#alias cerveau='ssh direct.auburndaletech.com -p 2222'

function vpnstatus {
	nordvpn status |grep Status | awk 'BEGIN { FS = ":" } ; {print $2}'	
}


#PS1="\e[1;32m\u\e[m (on) \e[1;32m\H\e[m (in) \e[1;32m(\w)\e[m\n -->> "
alias electr0n='ssh electr0n@161.97.67.197'


#function _update_ps1() {
#    PS1=$(powerline-shell $?)
#}

#source ~/code/pureline/pureline ~/.pureline.conf
#source ~/.bash/themes/2-column

      eval "$(starship init bash)"


#export THEME=$HOME/.bash/themes/agnoster-bash/agnoster.bash
#if [[ -f $THEME ]]; then
#    export DEFAULT_USER=`whoami`
#    source $THEME
#fi

#if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
#fi

# tabtab source for electron-forge package
# uninstall by removing these lines or running `tabtab uninstall electron-forge`
[ -f /home/electr0n/code/nordvpn-linux/node_modules/tabtab/.completions/electron-forge.bash ] && . /home/electr0n/code/nordvpn-linux/node_modules/tabtab/.completions/electron-forge.bash

alias ll='colorls -h -lA --sd --gs --group-directories-first'
alias ls='colorls -h --gs --group-directories-first'
alias external='cd /run/media/electr0n/ext-1tb/'
alias rsync-ebooks='rsync -uavP --progress /home/electr0n/books/  /run/media/electr0n/ext-1tb/books/'
source $(dirname $(gem which colorls))/tab_complete.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/electr0n/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
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

cat ~/logo2

alias del_react_files='rm -rf App.css index.css logo.svg reportWebVitals.js setupTests.js'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export NODE_HOME="$NVM_DIR/versions/node/v14.15.5/bin"

export PATH=$PATH:$NODE_HOME

    if [ -f "/home/electr0n/Downloads/tranalyzer2-0.8.8/scripts/t2_aliases" ]; then
        . "/home/electr0n/Downloads/tranalyzer2-0.8.8/scripts/t2_aliases"
    fi
