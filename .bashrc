
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s autocd

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

GIT_PROMPT_ONLY_IN_REPO=1
#source /usr/lib/bash-git-prompt/gitprompt.sh

PATH=$PATH:~/.scripts

export ZODIAC_MODULE_PATH='/home/jorri/dev/zodiac/tests'

alias sd='sudo '
alias em='emacs -nw '
alias vim='nvim '

alias ls='ls -h --color=auto --group-directories-first'
alias grep='grep --color=auto'

alias config='/usr/bin/git --git-dir=/home/jorri/.dotfiles/ --work-tree=/home/jorri'
alias mutt='neomutt '

alias xin='sudo xbps-install '
alias xq='xbps-query -Rs '
alias xrr='sudo xbps-remove -R '
alias xroo='sudo xbps-remove -Oo '

PATH=$PATH:~/dev/zodiac/build


#eval $(keychain --eval --quiet id_rsa)
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK
