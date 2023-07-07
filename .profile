export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"
export TERMINAL="urxvt"
export BROWSER="firefox"
export EDITOR="vim"
export RANGER_LOAD_DEFAULT_RC="FALSE"
#export ANDROID_HOME=$HOME/Android/sdk
export QEMU_AUDIO_DRV=pa
#export CUDA_DEBUGGER_SOFTWARE_PREEMPTION=1
export XDG_CONFIG_HOME="$HOME/.config"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:~/.emacs.d/bin
export PATH=$PATH:~/Programs/arm/bin
#export CLASSPATH='.;$HOME/.java/lib/ABLE.jar'

#GUROBI
export GUROBI_HOME="/opt/gurobi1001/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"

