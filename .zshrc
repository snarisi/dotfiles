# Source the .zshrc created by Zim
if [[ -f ~/.zshrc_zim ]]; then
    source ~/.zshrc_zim
fi


# Add some custom env vars; I don't know if there is another place, but that's fine
export DOTNET_ROOT=/usr/share/dotnet
export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
export PATH=${PATH}:${DOTNET_ROOT}


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


# Source your zshrc_local file, can't forget that
if [[ -f ~/.zshrc_local ]]; then
    source ~/.zshrc_local
fi
