# Source the .zshrc created by Zim
if [[ -f ~/.zshrc_zim ]]; then
    source ~/.zshrc_zim
fi

# Javascript
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Python
export PYENV_ROOT="$HOME/.pyenv"

# this is how you disable putting the name in parentheses
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

if [[ -d $PYENV_ROOT/bin ]]; then
     export PATH="$PYENV_ROOT/bin:$PATH"
     export PATH="$PYENV_ROOT/shims:$PATH"
     eval "$(pyenv init -)"
     eval "$(pyenv virtualenv-init -)"
fi

# Ruby
if [[ -d "$HOME/.rbenv" ]]; then
    export PATH="$PATH:$HOME/.rbenv/shims:$HOME/.rbenv/bin"
    eval "$(rbenv init -)"
fi

# Dotnet
# This one was different on Linux vs Mac because it installs to a different place.
if [[ -d /usr/local/share/dotnet ]]; then
    export DOTNET_ROOT=/usr/local/share/dotnet
elif [[ -d /usr/share/dotnet ]]; then
    export DOTNET_ROOT=/usr/share/dotnet
fi

export PATH=${PATH}:$HOME/.dotnet/tools

if [[ -x "$(command -v dotnet)" ]]; then
    export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
    export PATH=${PATH}:${DOTNET_ROOT}
fi

# Homebrew
if [[ -d "/home/linuxbrew/.linuxbrew" ]]; then
    export PATH=${PATH}:/home/linuxbrew/.linuxbrew/bin
fi

# I had to add this to get grip-mode working in emacs
export PATH=${PATH}:$HOME/.local/bin

# NOTE: You think you don't have to do this anymore because now you put the keyboard id in
# /etc/keyd/default.conf, and you have the service enabled all the time
# (using `sudo systemctl enable keyd`)
# actually the way you did it originally (with the apple keyboard id) was doing some weird
# things to the trackpad... try and figure that out
alias kk="sudo systemctl start keyd"
alias nk="sudo systemctl stop keyd"

# Source your zshrc_local file, can't forget that
if [[ -f "$HOME/.zshrc_local" ]]; then
    source ~/.zshrc_local
fi

# I'm just writing this here temporarily
#
# Instructions for fixing refind when mac breaks it:
#
# Boot into recovery mode (cmd-r) and turn off SIP (csrutil disable)
#
# Reboot into OSX and run:
#
# sudo mkdir /Volumes/ESP
# sudo mount -t msdos /dev/disk0s1 /Volumes/ESP
# sudo bless --mount /Volumes/ESP --setBoot --file /Volumes/ESP/EFI/refind/refind_x64.efi
#
# Boot into recovery mode (cmd-r) and turn on SIP (csrutil enable)
