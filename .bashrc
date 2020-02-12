#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

stty -ixon #Disable ctrl-s and ctrl-q
alias ls='ls --color=auto'
alias config='/usr/bin/git --git-dir=$HOME/.config.git --work-tree=$HOME'
PS1='[\u@\h \W]\$ '
