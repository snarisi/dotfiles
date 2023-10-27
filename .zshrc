# Source the .zshrc created by Zim
if [[ -f ~/.zshrc_zim ]]; then
    source ~/.zshrc_zim
fi


# Add some custom env vars; I don't know if there is another place, but that's fine
# This one was different on Linux vs Mac
# I changed the Linux one; figure out how to make it more ideal

if [[ -d /usr/local/share/dotnet ]]; then
    export DOTNET_ROOT=/usr/local/share/dotnet
elif [[ -d /usr/share/dotnet ]]; then
    export DOTNET_ROOT=/usr/share/dotnet
fi

if [[ -x "$(command -v dotnet)" ]]; then
    export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
    export PATH=${PATH}:${DOTNET_ROOT}
fi


# Enable changing of the caps lock key to control/escape. This will depend on
# whether you're using X11 or Wayland, so some will be commented out.

# If you're on X11:
# alias jfc="setxkbmap -option ctrl:nocaps && xcape -e 'Control_L=Escape'"
# jfc

# And if you're on Wayland:
# First you had to make a file, /etc/udev/hwdb.d/10-my-modifiers.hwdb:
#
# evdev:input:b0003*
#  KEYBOARD_KEY_70039=leftctrl  # bind capslock to control
#
# Then you had to run these commands, I think just once:
# sudo systemd-hwdb update
# sudo udevadm trigger
#
# And then you run this command, and you may need to install xcape:
if [[ -x "$(command -v xcape)" ]]; then
    xcape -e 'Control_L=Escape'
fi
# Note that will change your regular Control key into an Escape if it's tapped.


# Enable swapping of alt and windows keys (for your macbook)

# If you're on X11:
# alias fts="setxkbmap -option altwin:swap_lalt_lwin"
# fts

# And if you're on Wayland and you're on KDE you can:
# Go to System Preferences -> Input Devices -> Keyboard -> Advanced,
# and click on "Left Alt is swapped with Left Win"


# Javascript

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Python

if [[ -x "$(command -v pyenv)" ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi


# Source your zshrc_local file, can't forget that
if [[ -f ~/.zshrc_local ]]; then
    source ~/.zshrc_local
fi
