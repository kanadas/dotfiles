export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"
export TERMINAL="urxvt"
export BROWSER="firefox"
export EDITOR="vim"
export RANGER_LOAD_DEFAULT_RC="FALSE"
#export ANDROID_HOME=$HOME/Android/sdk
export QEMU_AUDIO_DRV=pa
export XDG_CONFIG_HOME="$HOME/.config"
alias config='/usr/bin/git --git-dir=$HOME/.config.git --work-tree=$HOME'
export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:~/.emacs.d/bin
export CLASSPATH='.;$HOME/.java/lib/ABLE.jar'
