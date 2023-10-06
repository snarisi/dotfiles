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
xcape -e 'Control_L=Escape'

# Enable swapping of alt and windows keys (for your macbook)
# TODO: Look up how to do this on Wayland
# alias fts="setxkbmap -option altwin:swap_lalt_lwin"
# fts

# Source your zshrc_local file, can't forget that
if [[ -f ~/.zshrc_local ]]; then
    source ~/.zshrc_local
fi
