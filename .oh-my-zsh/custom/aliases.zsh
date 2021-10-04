alias nfetch='neofetch --source ~/.config/neofetch/image/Archlinux-gray.png --backend kitty --colors 1 1'
alias pacrm="pacman -Qq | fzf --multi --preview 'pacman -Qi {1}' | xargs -ro sudo pacman -Rns"
alias t='todo.sh'
alias nv='nvim' 
alias vi='nvim' 
alias weather="curl 'wttr.in/?T'"
alias svim="sudoedit"
alias cht='f(){ curl -s "cheat.sh/$(echo -n "$*"|jq -sRr @uri)";};f'
alias rcd=". ranger"
alias gd="googler -n 4"
alias mdl="youtube-dl --add-metadata -xic -f bestaudio/best -o '~/tunes/%(title)s.%(ext)s' --audio-format mp3 --embed-thumbnail"
alias tncmpcpp='tmux new-session "tmux source-file ~/.ncmpcpp/tmux_session"'
alias clip="xclip -sel clip"
alias logout="kill -9 -1"
alias ttcmd="echo '' | fzf -q '$*' --prompt '│ ' --pointer '― ' --preview-window=up:99% --preview='eval {q}'"
alias fet="info='n user os sh wm kern pkgs mem term col' separator=' | ' accent='2' fet.sh"
alias fm="setsid -f thunar"
alias c="bc -l"
alias yi="yay -Slq | fzf -m --preview 'yay -Si {1}' | xargs -ro yay -S"
alias cat="bat"
alias pfetch="curl -s https://raw.githubusercontent.com/dylanaraps/pfetch/master/pfetch | sh"
alias nav="navi --fzf-overrides '--color fg:7,bg:-1,hl:6,fg+:6,bg+:-1,hl+:6,info:2,prompt:1,spinner:5,pointer:5,marker:3,header:8'"
alias xmr="xmonad --restart"
alias xmc="xmonad --recompile"
alias topmem='watch ps -e -o pid,cmd,%cpu,%mem --sort=-%mem | head -n 6'
alias icat="kitty +kitten icat"
alias ll='lsd -Al'
alias ls='lsd -A'
alias use='xrdb merge ~/.Xresources'
alias load='kill -USR1 $(pidof st)'
alias disks='echo "╓───── d i s k . u s a g e";\
			       echo "╙────────────────────────────────────── ─ ─ "; \
			       df -h;'
alias cpu='watch ps -Ao user,uid,comm,pid,pcpu,tty --sort=-pcpu | head -n 6'


# SSH mgmt 

alias hunt='ssh electr0n@hunt.revrse.sh'
alias lab='ssh root@lab.revrse.sh'
alias docker.lab='ssh root@docker.lab.revrse.sh'
alias logs.lab='ssh root@logs.lab.revrse.sh'
alias dev='ssh electr0n@dev.revrse.sh'
alias deploy='rsync -crvz _site hunt.revrse.sh:~/hunt.revrse.sh/'
alias cursor="printf '\033[3 q'"
alias -g C='| wc -l'
alias -g H='| head'
alias -g L="| less"
alias -g N="| /dev/null"
alias -g S='| sort'
alias -g G='| grep'
