export WHAT=what

# emacs-related stuff
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) ]]; then
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
fi

# git-related completion in the shell
if [[ -f ~/.git-completion.zsh ]]; then
    . ~/.git-completion.zsh
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
# NOTE: This is from the site https://www.themoderncoder.com/add-git-branch-information-to-your-zsh-prompt/

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats 'on %b'

# Find and set branch name var if in git repository.
function git_branch_name()
{
  branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
  if [[ $branch == "" ]];
  then
    :
  else
    echo ' \e[0;32m['$branch']\e[0m'
  fi
}

# Enable substitution in the prompt.
setopt prompt_subst

# Config for prompt. PS1 synonym.
autoload -U colors && colors
prompt='%1/$( git_branch_name) '
# End the part from that site... you should look at it again, you may be to delete stuff

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

# Tell pyenv-virtualenvwrapper to use pyenv when creating new Python environments
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

# Set the pyenv shims to initialize
if command -v pyenv 1>/dev/null 2>&1; then
 eval "$(pyenv init -)"
fi

# You might need to this to get your path straight?
# TODO: See if it can mess anything up
export PATH=~/.pyenv/shims:$PATH

# get `workon`, `makevirtualenv`, etc. to work... see the thing at the end that brew said to do
export VIRTUALENV_WRAPPER_PYTHON=/usr/bin/python3
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export WORKON_HOME=$HOME/.virtualenvs

# NOTE: If you go to a directory and run `setvirtualenvproject`, it will move to
# the directory you are in when you workon the project next time

# Says https://docs.python.org/3/using/cmdline.html:
# If this is set to a non-empty string, Python won’t try to write .pyc files on the import of source
# modules. This is equivalent to specifying the -B option.
export PYTHONDONTWRITEBYTECODE=x

# Says: https://stackoverflow.com/questions/18419500/how-to-make-mac-os-use-the-python-installed-by-homebrew
# Get the system to use a version of python installed by homebrew:
export PATH=/usr/local/share/python:/usr/local/bin:$PATH

# the thing i dowloaded from homebrew (`brew search virtualenv`) said to put this near the end
source virtualenvwrapper.sh

# Get MacPorts to work, I think:
export PATH=/opt/local/bin:$PATH

# source your zshrc_local file, can't forget that
if [[ -f ~/.zshrc_local ]]; then
    source ~/.zshrc_local
fi

# this was added automatically, by rustup, I believe?
. "$HOME/.cargo/env"