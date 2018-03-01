export PATH=/usr/local/share/python:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
export PYTHONDONTWRITEBYTECODE=x
export DEVELOPMENT_MODE=1
export DEV_MODE=1

export PATH=~/nand2tetris/tools:$PATH
export PATH=/opt/local/bin:$PATH
export PATH="$HOME/Library/Python/2.7/bin/":$PATH

if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
	  -f `brew --prefix`/etc/bash_completion ]]; then
	. `brew --prefix`/etc/bash_completion
fi

if [ -f ~/.git-completion.bash ]; then
	. ~/.git-completion.bash
fi

# Turn on 256 color support...
if [ "x$TERM" = "xxterm"  ]; then
	export TERM="xterm-256color"
fi

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
export PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"

READLINE_DIR=/usr/local/opt/readline
OPENSSL_DIR=/usr/local/opt/openssl

export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

source /usr/local/bin/virtualenvwrapper.sh
VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

source $(which virtualenvwrapper.sh)

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

source ~/.git-prompt.sh
PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '

alias cl="clear"
alias tm="bash ~/tmstart"

function em () {
    emacsclient -nw "$1"
}

export PYTHONSTARTUP=~/.pystartup

if [ -f ~/.bashrc_local ]; then
	source ~/.bashrc_local
fi
export PATH="/usr/local/opt/redis@2.8/bin:$PATH"

# better history
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development
source /Users/samnarisi/.yy/bin/y

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
