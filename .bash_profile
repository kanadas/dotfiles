#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
PATH=$PATH:~/.scripts
export BROWSER="firefox"
export TERMINAL="urxvt"
export RANGER_LOAD_DEFAULT_RC="FALSE"
export ANDROID_HOME=$HOME/Android/sdk
export QEMU_AUDIO_DRV=pa
alias config='/usr/bin/git --git-dir=$HOME/.config.git --work-tree=$HOME'
#export PATH=$PATH:$ANDROID_HOME/emulator
#export PATH=$PATH:$ANDROID_HOME/tools
#export PATH=$PATH:$ANDROID_HOME/tools/bin
#export PATH=$PATH:$ANDROID_HOME/platform-tools
#export PATH=$PATH:/home/tkanas/.local/bin
#export PATH=$PATH:/home/tkanas/.stack/programs/x86_64-linux/ghc-tinfo6-8.6.4/bin
