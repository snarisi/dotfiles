# Get the thing to stop complaining that it's using zsh now
export BASH_SILENCE_DEPRECATION_WARNING=1

# Emacs-related stuff
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) ]]; then
	[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
fi

# Git-related completion in the shell
if [[ -f ~/.git-completion.bash ]]; then
	. ~/.git-completion.bash
fi

# Turn on 256 color support...
if [[ "x$TERM" = "xxterm"  ]]; then
	export TERM="xterm-256color"
fi

# not sure what this is, actually
export PYTHONDONTWRITEBYTECODE=x

# or this
READLINE_DIR=/usr/local/opt/readline
OPENSSL_DIR=/usr/local/opt/openssl

# i think this gets your `workon some_project` to work
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Development
export VIRTUALENVWRAPPER_PYTHON=~/.pyenv/shims/python
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
source /usr/local/bin/virtualenvwrapper.sh

# not really sure of these either
export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

# these are yipit things
export DEV_MODE=1
export PROXY_CENTRAL_SERVICE='local-testing'
export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development

# this is how yipit says to set python now
# export PATH="/usr/local/opt/python/libexec/bin:$PATH"
export PATH=/usr/local/share/python:/usr/local/bin:$PATH
export PYTHONDONTWRITEBYTECODE=x
export DEV_MODE=1
export PROXY_CENTRAL_SERVICE='local-testing'

READLINE_DIR=/usr/local/opt/readline
OPENSSL_DIR=/usr/local/opt/openssl

export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# It should be the second one of these...
# eval "$(pyenv init -)"
eval "$(pyenv init --path)"

if [[ -f ~/.bashrc_local ]]; then

    source ~/.bashrc_local
fi

source ~/.git-prompt.sh
PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '

alias cl="clear"
alias tm="bash ~/tmstart"

alias macs='emacsclient -a "" -c -n'
alias tmacs='emacsclient -a "" -t'
alias emacs_unfreeze='killall -USR2 Emacs'

export EDITOR='emacsclient -a "" -t'

# better history
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# added by travis gem
[[ -f /Users/samnarisi/.travis/travis.sh ]] && source /Users/samnarisi/.travis/travis.sh
alias ec2_ssh='ssh -o StrictHostKeyChecking=no -i ~/.ssh/yipit_staging_1.pem -l ec2-user'

new-emacs-workspace ()
{
	NAME=$(echo $1 | tr a-z A-Z)
    emacs --daemon=$NAME && emacsclient -s $NAME -c -n
}

emacsclient-list-sockets ()
{
	ls "${TMPDIR-/tmp}/emacs$(id -u)"
}

if [[ -s "/Users/samnarisi/.yy/bin/y" ]]; then
    alias yy='unalias yy && source /Users/samnarisi/.yy/bin/y && yy'
fi

export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development

if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

. "$HOME/.cargo/env"
