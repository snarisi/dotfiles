# get the thing to stop complaining that it's using zsh now
export BASH_SILENCE_DEPRECATION_WARNING=1

# # not really sure what these are for... but keep them until you figure it out
# export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
# export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
# export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

# make your command line have the git branch in it, I think...
source ~/.git-prompt.sh
PS1='\[\e[0m\]\W\[\e[0m\]$(__git_ps1 " \[\e[32m\][%s]\[\e[0m\]")\[\e[0m\] ●\[\e[0m\] '

# make emacs the default editor, I think...
export EDITOR='emacsclient -a "" -t'

# better history...
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

# some node version manager stuff...
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# TODO: I don't know if this is correct, but it complains otherwise
# `> /dev/null` is how to get it to not to write anything out, apparently
nvm use default > /dev/null

# i forget what avn is...
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# pyenv stuff... I suppose it's important...
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# Set the pyenv shims to initialize
# And you downloaded the extension by running:
# `git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv`
# Also note: If you go to a directory and run `setvirtualenvproject`, it will move to
# the directory you are in when you workon the project next time
# Or it will workon when you change directories, I forget which
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# You might need this to get your path straight?
# TODO: See if it can mess anything up
export PATH=~/.pyenv/shims:$PATH

# Says https://docs.python.org/3/using/cmdline.html:
# If this is set to a non-empty string, Python won’t try to write .pyc files on the import of source
# modules. This is equivalent to specifying the -B option.
export PYTHONDONTWRITEBYTECODE=x

# the thing i dowloaded from homebrew (`brew search virtualenv`) said to put this near the end
# NOTE: You commented out the next line on linux
# source virtualenvwrapper.sh
export VIRTUALENVWRAPPER_VIRTUALENV=/home/snarisi/.pyenv/shims/virtualenv

# NOTE: You had to set up https://github.com/wez/evremap
# sudo cp evremap.service /usr/lib/systemd/system/
# sudo systemctl daemon-reload
# sudo systemctl start evremap.service
# Also I think that I don't have to do this, because i ran
# `sudo systemctl enable evremap.service`, but just leave it case
alias kk="sudo systemctl start evremap.service"
alias nk="sudo systemctl stop evremap.service"

# source your bashrc_local file, can't forget that
if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

# this was added automatically, by rustup, I believe?
. "$HOME/.cargo/env"
