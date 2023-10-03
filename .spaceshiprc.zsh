#-- some spaceship setting
SPACESHIP_TIME_SHOW=true
SPACESHIP_PROMPT_ADD_NEWLINE=true
SPACESHIP_PROMPT_SEPERATE_LINE=true
SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=false


#-- some pyenv settings
# Set the pyenv shims to initialize
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# FYI, you downloaded the extension by running: git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
if which pyenv-virtualenv-init > /dev/null; then
    eval "$(pyenv virtualenv-init -)";
fi


# -- some node version manager stuff
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
