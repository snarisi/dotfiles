export PATH=/usr/local/share/python:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
export PYTHONDONTWRITEBYTECODE=x
export DEVELOPMENT_MODE=1
export DEV_MODE=1

export PATH=~/nand2tetris/tools:$PATH
export PATH=/opt/local/bin:$PATH

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

if [ -f ~/.bashrc_local ]; then
	source ~/.bashrc_local
fi
