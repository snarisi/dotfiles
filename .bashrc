# (put this here for now... figure out where to move it to)
# I had some trouble installing python 3.9.5 because the command would, so run:
#
# CC=gcc pyenv install 3.9.5
#
# That gets it to use gcc instead. I have no idea why.

# Get the thing to stop complaining that it's using zsh now
export BASH_SILENCE_DEPRECATION_WARNING=1

# Emacs-related stuff
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) ]]; then
	[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
fi

if [[ -f ~/.git-completion.bash ]]; then
	. ~/.git-completion.bash
fi

# Turn on 256 color support...
if [[ "x$TERM" = "xxterm"  ]]; then
	export TERM="xterm-256color"
fi

# Copied from what Yipit said to do
export PATH="/usr/local/opt/python/libexec/bin:$PATH"
export PYTHONDONTWRITEBYTECODE=x
export DEV_MODE=1

# Yipit says to put this in here, probably worth making a note of it
export PROXY_CENTRAL_SERVICE='local-testing'

# This was only for older versions of homebrew, according to Yipit
# export PATH=/usr/local/share/python:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.local/bin:$PATH

# Leave this commented out for now...
# eval "$(pyenv init -)"
# python3.latest() {
#   pyenv shell 3.9.5
#   pyenv virtualenvwrapper
# }
# python3.latest

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Development

# NOTE: I had it this way, but the current Yipit docs say to do it the other way,
# so let's try that
# export VIRTUALENVWRAPPER_PYTHON=/Users/samnarisi/.pyenv/versions/3.9.5/bin/python

# Is this the right way? It used to be just `/python`
# export VIRTUALENVWRAPPER_PYTHON=/usr/local/opt/python/libexec/bin/python
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3.9

# Likewise with this one
# export VIRTUALENVWRAPPER_VIRTUALENV=/Users/samnarisi/.pyenv/versions/3.9.5/bin/virtualenv
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv

# And this one
# source /opt/homebrew/bin/virtualenvwrapper.sh
source /usr/local/bin/virtualenvwrapper.sh

eval "$(pyenv init -)"

# export PYENV_ROOT="$HOME/.pyenv"
# export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init --path)"

# I don't know this ever was there...
# pip3 install virtualenv virtualenvwrapper

# Done copying what Yipit said

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
# export PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"

# READLINE_DIR=/usr/local/opt/readline
# OPENSSL_DIR=/usr/local/opt/openssl

# export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib -L/usr/local/opt/mysql@5.7/lib"
# export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include -I/usr/local/opt/mysql@5.7/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

# source /usr/local/bin/virtualenvwrapper.sh
# export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

source ~/.git-prompt.sh
PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '

alias cl="clear"
alias tm="bash ~/tmstart"

alias macs='emacsclient -a "" -c -n'
alias tmacs='emacsclient -a "" -t'
alias emacs_unfreeze='killall -USR2 Emacs'

export EDITOR='emacsclient -a "" -t'

export PYTHONSTARTUP=~/.pystartup

# better history
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

# https://askubuntu.com/questions/67283/is-it-possible-to-make-writing-to-bash-history-immediate
# shopt -s histappend
# PROMPT_COMMAND='history -a;history -n'

export NVM_DIR="$HOME/.nvm"
[[ -s "/usr/local/opt/nvm/nvm.sh" ]] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[[ -s "/usr/local/opt/nvm/etc/bash_completion" ]] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# [[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn
# stole from https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
# if [ -s "$HOME/.nvm/nvm.sh" ] && [ ! "$(type -t __init_nvm)" = function ]; then
#     export NVM_DIR="$HOME/.nvm"
#     declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack' 'webpack-dev-server')
#     function __init_nvm() {
# 	for i in "${__node_commands[@]}"; do unalias $i; done
# 	. "$NVM_DIR"/nvm.sh
# 	[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# 	unset __node_commands
# 	unset -f __init_nvm
#     }
#     for i in "${__node_commands[@]}"; do alias $i='__init_nvm && '$i; done
# fi

# added by travis gem
[[ -f /Users/samnarisi/.travis/travis.sh ]] && source /Users/samnarisi/.travis/travis.sh
alias ec2_ssh='ssh -o StrictHostKeyChecking=no -i ~/.ssh/yipit_staging_1.pem -l ec2-user'

# copies everything currently in env to the emacs process
# does it fuck anything up?
# env-to-emacs ()
# {
# 	SERVER_NAME=$1
#     env | awk -F '=' '{print "(setenv \"" $1 "\" \"" $2 "\")"}' | while read line; do emacsclient -s "$SERVER_NAME" -e "$line"; done
# }

new-emacs-workspace ()
{
	NAME=$(echo $1 | tr a-z A-Z)
    emacs --daemon=$NAME && emacsclient -s $NAME -c -n
}

emacsclient-list-sockets ()
{
	ls "${TMPDIR-/tmp}/emacs$(id -u)"
}

# stole idea from https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
# trying to defer loading these things so opening shells doesn't take so long
# if [[ -s $(which virtualenvwrapper.sh) ]] && [[ ! "$(type -t __init_venv)" = function ]]; then
#     export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
#     declare -a __venv_commands=('workon' 'mkvirtualenv' 'rmvirtualenv')
#     function __init_venv() {
# 	for i in "${__venv_commands[@]}"; do unalias $i; done
# 	source $(which virtualenvwrapper.sh)
# 	unset __venv_commands
# 	unset -f __init_venv
#     }
#     for i in "${__venv_commands[@]}"; do alias $i='__init_venv && '$i; done
# fi

if [[ -s "/Users/samnarisi/.yy/bin/y" ]]; then
    alias yy='unalias yy && source /Users/samnarisi/.yy/bin/y && yy'
fi

# export PYENV_ROOT="$HOME/.pyenv"
# export PATH="$PYENV_ROOT/bin:$PATH"
# if command -v pyenv 1>/dev/null 2>&1; then
#   eval "$(pyenv init -)"
#   # pyenv virtualenvwrapper_lazy
# fi

if [[ -f ~/.bashrc_local ]]; then
	source ~/.bashrc_local
fi

# https://github.com/akermu/emacs-libvterm/pull/126
# vterm_prompt_end() {
#   printf "\e]51;A$(pwd)\e\\"
# }
# PS1=$PS1'$(vterm_prompt_end)'

export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development

# from https://github.com/pyenv/pyenv/issues/1740
export PATH="$HOME/.pyenv/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
