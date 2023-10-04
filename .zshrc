# Source the .zshrc created by Zim
if [[ -f ~/.zshrc_zim ]]; then
    source ~/.zshrc_zim
fi

# Add some custom env vars; I don't know if there is another place, but that's fine
export DOTNET_ROOT=/usr/share/dotnet
export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
export PATH=${PATH}:${DOTNET_ROOT}

# Enable changing of the caps lock key to control/escape
alias jfc="setxkbmap -option ctrl:nocaps && xcape -e 'Control_L=Escape'"
# jfc

# Enable swapping of alt and windows keys (for your macbook)
alias fts="setxkbmap -option altwin:swap_lalt_lwin"
# fts

# Source your zshrc_local file, can't forget that
if [[ -f ~/.zshrc_local ]]; then
    source ~/.zshrc_local
fi
