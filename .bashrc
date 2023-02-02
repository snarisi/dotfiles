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

# NOTE: I believe I can delete this part

# I believe this is a yipit thing, so I'll comment it out for now
# export DEV_MODE=1

# This is a yipit thing, so I'll comment it out for now
# export PROXY_CENTRAL_SERVICE='local-testing'

# displays the running emacs sockets, which is just "server" now unless you have multiple windows
# comment it out for now
# emacsclient-list-sockets ()
# {
#     ls "${TMPDIR-/tmp}/emacs$(id -u)"
# }

# i forget why this function is here... i guess i used to want to run multiple emacs windows?
# comment it out for now
# new-emacs-workspace ()
# {
#     NAME=$(echo $1 | tr a-z A-Z)

#     emacs --daemon=$NAME && emacsclient -s $NAME -c -n
# }

# some yipit stuff, you should be fine to delete
# added by travis gem
# [[ -f /Users/samnarisi/.travis/travis.sh ]] && source /Users/samnarisi/.travis/travis.sh
# alias ec2_ssh='ssh -o StrictHostKeyChecking=no -i ~/.ssh/yipit_staging_1.pem -l ec2-user'

# some yy stuff, i don't remember why though - you should be fine to delete
# if [[ -s "/Users/samnarisi/.yy/bin/y" ]]; then
#     alias yy='unalias yy && source /Users/samnarisi/.yy/bin/y && yy'
# fi

# NOTE: This entire -- C code section -- has a bunch of stuff I'm going to try
# to comment out
# # i think these are C related, but I am not sure
# export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
# export CFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
# export PKG_CONFIG_PATH="/usr/local/opt/zlib/lib/pkgconfig"

# # NOTE: I don't know what libpq is, but I believe this is from homebrew
# # For compilers to find libpq you may need to set:
# export LDFLAGS="-L/usr/local/opt/libpq/lib"
# export CPPFLAGS="-I/usr/local/opt/libpq/include"

# # For pkg-config to find libpq you may need to set:
# export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig"

# # someone on github said this which you listened to and you think it worked:
# # I am using Mac os big sure and this works for me.
# # install openssl using brew

# # brew install openssl
# # after that export these variables in the terminal.

# # export LDFLAGS="-L/usr/local/opt/openssl/lib"

# # keep this one in there, because installing cryptography failed without it
# export CPPFLAGS="-I/usr/local/opt/openssl/include"

# # then finally install psycopg2

# # pip3 install psycopg2

# # this is necessary to get psycopg2 to install and maybe some other things...
# # note that you can probably delete the commented out stuff above
# LDFLAGS="-I/usr/local/opt/openssl/include -L/usr/local/opt/openssl/lib"

# # homebrew said this when you upgraded by the way...
# # so uncomment it out if you need to
# # If you need to have libxml2 first in your PATH, run:
# #   echo 'export PATH="/usr/local/opt/libxml2/bin:$PATH"' >> /Users/samnarisi/.bash_profile

# # For compilers to find libxml2 you may need to set:
# #   export LDFLAGS="-L/usr/local/opt/libxml2/lib"
# #   export CPPFLAGS="-I/usr/local/opt/libxml2/include"

# # For pkg-config to find libxml2 you may need to set:
# #   export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
# # -- end of homebrew stuff
# end - C section -

# # NOTE: Does doing this overwrite the ones above?
# export LDFLAGS="-L/usr/local/opt/libffi/lib"
# export CPPFLAGS="-I/usr/local/opt/libffi/include"

# NOTE: I don't think you need these anymore, delete at your leisure
# These are yipit things, I think I can delete them
# export Y_PATH=/Users/samnarisi/.yy
# export Y_DEV_PATH=/Users/samnarisi/Development

# leave these ones commented out... I think it's not needed
# export VIRTUALENVWRAPPER_PYTHON=~/.pyenv/shims/python
# export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
# source /usr/local/bin/virtualenvwrapper.sh

# NOTE: This should no longer be true
# by the way, you do virtualenvs now by running `pyenv activate [VENV_NAME]`,
# and you create the virtualenv like `pyenv virtualenv [VENV_NAME]`
# or see the `wk` function below...

# NOTE: homebrew said I might need to do this...
# We've installed your MySQL database without a root password. To secure it run:
#     mysql_secure_installation

# MySQL is configured to only allow connections from localhost by default

# To connect run:
#     mysql -u root

# mysql@5.7 is keg-only, which means it was not symlinked into /usr/local,
# because this is an alternate version of another formula.

# If you need to have mysql@5.7 first in your PATH, run:
#   echo 'export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"' >> /Users/samnarisi/.bash_profile

# For compilers to find mysql@5.7 you may need to set:
#   export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"
#   export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"

# To restart mysql@5.7 after an upgrade:
#   brew services restart mysql@5.7
# Or, if you don't want/need a background service you can just run:
#   /usr/local/opt/mysql@5.7/bin/mysqld_safe --datadir=/usr/local/var/mysql
# - end what i might need to do

# TODO: I am trying this for now...
# export PATH="/Library/Frameworks/Mono.framework/Versions/Current/bin:$PATH

# make an alias for the new virtualenv thing
# one thing is it will say `pyenv-virtualenv: no virtualenv has been activated.` when
# you're not in a virtualenv to start, but it doesn't break or anything
# function wk ()
# {
#     cd $HOME/Development/$1;
#     pyenv deactivate;
#     pyenv activate $1;
#     [ -f "./.nvmrc" ] && { nvm use; }
# }

# # just cause I can't decide which one I like better yet
# alias wkon=wk

# # TODO: figure out a better way to do this...
# function wkpersonal ()
# {
#     cd $HOME/Personal/$1;
#     pyenv deactivate;
#     pyenv activate $1;
#     [ -f "./.nvmrc" ] && { nvm use; }
# }

# alias wkp=wkpersonal
# NOTE: That note ends here

# this was output by homebrew when you installed libpq, remember to do something about it
# libpq is keg-only, which means it was not symlinked into /usr/local,
# because conflicts with postgres formula.

# If you need to have libpq first in your PATH, run:
# echo 'export PATH="/usr/local/opt/libpq/bin:$PATH"' >> /Users/samnarisi/.bash_profile

# this will disable the "(virtualenv-name)" thing from appearing in your shell,
# which they are apparently getting rid of for some reason... comment it out for now
# export PYENV_VIRTUALENV_DISABLE_PROMPT=1
# END NOTE: I believe I can delete this humungous part

# source your bashrc_local file, can't forget that
if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

# this was added automatically, by rustup, I believe?
. "$HOME/.cargo/env"
