# get the thing to stop complaining that it's using zsh now
export BASH_SILENCE_DEPRECATION_WARNING=1

# emacs-related stuff
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) ]]; then
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
fi

# git-related completion in the shell
if [[ -f ~/.git-completion.bash ]]; then
    . ~/.git-completion.bash
fi

# turn on 256 color support...
if [[ "x$TERM" = "xxterm"  ]]; then
    export TERM="xterm-256color"
fi

# not sure what this is, actually
export PYTHONDONTWRITEBYTECODE=x

# or this
# READLINE_DIR=/usr/local/opt/readline
# OPENSSL_DIR=/usr/local/opt/openssl

# i think this gets your `workon some_project` to work
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Development

# leave these ones commented out... I think it's not needed
# export VIRTUALENVWRAPPER_PYTHON=~/.pyenv/shims/python
# export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
# source /usr/local/bin/virtualenvwrapper.sh

# by the way, you do virtualenvs now by running `pyenv activate [VENV_NAME]`,
# and you create the virtualenv like `pyenv virtualenv [VENV_NAME]`
# or see the `wk` function below...

# not really sure of these either
export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

# these are yipit things
export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development

# this is how yipit says to set python now... though I guess it broke something
# export PATH="/usr/local/opt/python/libexec/bin:$PATH"

export PATH=/usr/local/share/python:/usr/local/bin:$PATH
export PYTHONDONTWRITEBYTECODE=x
export DEV_MODE=1
export PROXY_CENTRAL_SERVICE='local-testing'

# i forget why this is here... comment out for now
# READLINE_DIR=/usr/local/opt/readline
# OPENSSL_DIR=/usr/local/opt/openssl

# pyenv stuff... I suppose it's important
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# keep these commeneted out in favor of the next one
# eval "$(pyenv init -)"
# eval "$(pyenv init --path)"

# note: i found these here: https://stackoverflow.com/questions/66482346/problems-installing-python-3-6-with-pyenv-on-mac-os-big-sur
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# i think these are C related, but I am not sure
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"

# make your command line have the git branch in it, I think
source ~/.git-prompt.sh
PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '

# just some aliases... I forget what `tm` is, actually
alias cl="clear"
alias tm="bash ~/tmstart"
alias macs='emacsclient -a "" -c -n'
alias tmacs='emacsclient -a "" -t'
alias emacs_unfreeze='killall -USR2 Emacs'

# make emacs the default editor, i think
export EDITOR='emacsclient -a "" -t'

# better history
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

# some node version manager stuff
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# i forget what avn is
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# added by travis gem
[[ -f /Users/samnarisi/.travis/travis.sh ]] && source /Users/samnarisi/.travis/travis.sh
alias ec2_ssh='ssh -o StrictHostKeyChecking=no -i ~/.ssh/yipit_staging_1.pem -l ec2-user'

# i forget why this function is here... i guess i used to want to run multiple emacs windows?
new-emacs-workspace ()
{
    NAME=$(echo $1 | tr a-z A-Z)
    emacs --daemon=$NAME && emacsclient -s $NAME -c -n
}

# displays the running emacs sockets, which is just "server" now unless you have multiple windows
emacsclient-list-sockets ()
{
    ls "${TMPDIR-/tmp}/emacs$(id -u)"
}

# some yy stuff, i don't remember why though
if [[ -s "/Users/samnarisi/.yy/bin/y" ]]; then
    alias yy='unalias yy && source /Users/samnarisi/.yy/bin/y && yy'
fi

export Y_PATH=/Users/samnarisi/.yy
export Y_DEV_PATH=/Users/samnarisi/Development

# make an alias for the new virtualenv thing
# it should probably check that there is an .nvmrc file before doing that last bit
# and i should make it search through several directories, but this is fine for now
function wk ()
{
    cd $HOME/Development/$1;
    pyenv activate $1;
    [ -f "./.nvmrc" ] && { nvm use; }
}

# just cause I can't decide which one I like better yet
alias wkon=wk

# this will disable the "(virtualenv-name)" thing from appearing in your shell,
# which they are apparently getting rid of for some reason... comment it out for now
# export PYENV_VIRTUALENV_DISABLE_PROMPT=1

# source your bashrc_local file, can't forget that
if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

# this was added automatically, I forget by what
. "$HOME/.cargo/env"
