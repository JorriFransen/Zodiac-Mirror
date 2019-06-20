#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s autocd

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

GIT_PROMPT_ONLY_IN_REPO=1
source /usr/lib/bash-git-prompt/gitprompt.sh

PATH=$PATH:~/.scripts

export ZODIAC_PATH='/home/jorri/dev/zodiac/tests'
# PATH=$PATH:'~/dev/zodiac/build'


alias em='emacs -nw '
alias vim='nvim '
alias packer='packer-aur '

alias ls='ls -h --color=auto --group-directories-first'
alias grep='grep --color=auto'

alias config='/usr/bin/git --git-dir=/home/jorri/.dotfiles/ --work-tree=/home/jorri'
