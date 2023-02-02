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

# i forget why this is here...
READLINE_DIR=/usr/local/opt/readline
OPENSSL_DIR=/usr/local/opt/openssl

# not really sure what these are for either...
export LDFLAGS="-L$READLINE_DIR/lib -L$OPENSSL_DIR/lib"
export CFLAGS="-I$READLINE_DIR/include -I$OPENSSL_DIR/include"
export SWIG_FEATURES="-cpperraswarn -includeall -I$OPENSSL_DIR/include"

# make your command line have the git branch in it, I think...
source ~/.git-prompt.sh
PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '

# just some aliases... I forget what `tm` is, actually...
alias cl="clear"
alias tm="bash ~/tmstart"
alias macs='emacsclient -a "" -c -n'
alias tmacs='emacsclient -a "" -t'
alias emacs_unfreeze='killall -USR2 Emacs'

# make emacs the default editor, i think...
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

# i forget what avn is...
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# pyenv stuff... I suppose it's important...
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# note: i found these here: https://stackoverflow.com/questions/66482346/problems-installing-python-3-6-with-pyenv-on-mac-os-big-sur
eval "$(pyenv init -)"

# I don't know if I need this anymore... test removing it sometime...
# eval "$(pyenv virtualenv-init -)"

# get `workon`, `makevirtualenv`, etc. to work...
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export WORKON_HOME=$HOME/.virtualenvs
pyenv virtualenvwrapper_lazy

# NOTE: If you go to a directory and run `setvirtualenvproject`, it will move to
# the directory you are in when you workon the project next time

# Says https://docs.python.org/3/using/cmdline.html:
# If this is set to a non-empty string, Python won’t try to write .pyc files on the import of source
# modules. This is equivalent to specifying the -B option.
export PYTHONDONTWRITEBYTECODE=x

# Says: https://stackoverflow.com/questions/18419500/how-to-make-mac-os-use-the-python-installed-by-homebrew
# Get the system to use a version of python installed by homebrew:
export PATH=/usr/local/share/python:/usr/local/bin:$PATH

# source your bashrc_local file, can't forget that
if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

# this was added automatically, by rustup, I believe?
. "$HOME/.cargo/env"
