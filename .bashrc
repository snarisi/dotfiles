export PATH=/usr/local/share/python:/usr/local/bin:$PATH
export PYTHONDONTWRITEBYTECODE=x
export DEVELOPMENT_MODE=1
export DEV_MODE=1

READLINE_DIR=/usr/local/opt/readline
OPENSSL_DIR=/usr/local/opt/openssl

export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

source /usr/local/bin/virtualenvwrapper.sh
VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

source $(which virtualenvwrapper.sh)
export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development
export GITHUB_OAUTH_TOKEN=bc874dd7b57cd922757c86a64ccdefe814a381a8

source /Users/samnarisi/.yy/bin/y

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

alias cl="clear"
alias tm="bash ~/tmstart"
