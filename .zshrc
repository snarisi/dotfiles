# Make your command line have the git branch in it, I think...
source ~/.git-prompt.sh

# Put the git branch and status in the command line
autoload -Uz vcs_info

precmd_vcs_info() { vcs_info }
precmd_functions+=(precmd_vcs_info)
setopt prompt_subst

zstyle ':vcs_info:git*' formats "  %F{green}%b%f%m%u%c%a"
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr ' %F{yellow}✚%f'
zstyle ':vcs_info:*' unstagedstr ' %F{red}●%f'

PROMPT='%F{blue}%}%9c%{%F{green}%}${vcs_info_msg_0_}%F{blue} $%F{none} '

# Make emacs the default editor, I think...
export EDITOR='emacsclient -a "" -t'

# Better history...
export HISTIGNORE="ls:ll:cd:pwd"
export HISTFILESIZE=10000 # maybe too much?
export HISTSIZE=10000 # maybe too much?
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="[$(tput setaf 6)%F %T$(tput sgr0)]: " # colorful date

# Get some colors when you ls...
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Some node version manager stuff...
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# The terminal complains otherwise
# `> /dev/null` is how to get it to not to write anything out, apparently
nvm use default > /dev/null

# I forget what avn is...
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
# `If this is set to a non-empty string, Python won’t try to write .pyc files on the
# import of source modules. This is equivalent to specifying the -B option.`
export PYTHONDONTWRITEBYTECODE=x

# I will try putting this here to use virtualenv
export VIRTUALENVWRAPPER_VIRTUALENV=/home/snarisi/.pyenv/shims/virtualenv

# NOTE: You had to set up https://github.com/wez/evremap
# sudo cp evremap.service /usr/lib/systemd/system/
# sudo systemctl daemon-reload
# sudo systemctl start evremap.service
# Also I think that I don't have to do this, because i ran
# `sudo systemctl enable evremap.service`, but just leave it in case
alias kk="sudo systemctl start evremap.service"
alias nk="sudo systemctl stop evremap.service"

# Alias for turning your laptop monitor off
alias nm="kscreen-doctor output.eDP-1.disable"

# Alias for turning your laptop monitor back on
alias mm="kscreen-doctor output.eDP-1.enable"

# Source your bashrc_local file, can't forget that
if [[ -f ~/.bashrc_local ]]; then
    source ~/.bashrc_local
fi

# This was added automatically, by rustup, I believe?
# . "$HOME/.cargo/env"

. "$HOME/.local/bin/env"
