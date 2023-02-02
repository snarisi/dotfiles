if [[ -f ~/.bashrc ]]; then
	source ~/.bashrc
fi

if [[ -f `brew --prefix`/etc/bash_completion.d/brew ]]; then
    source `brew --prefix`/etc/bash_completion.d/brew
fi

# Remove this because it's in your .bashrc
# This should make it stop warning that the default is now zsh
# export BASH_SILENCE_DEPRECATION_WARNING=1

# NOTE: I think I can delete this
# . "$HOME/.cargo/env"

# NOTE: I think I can delete these
# export PATH="/usr/local/opt/libpq/bin:$PATH"
# export PATH="/usr/local/opt/libpq/bin:$PATH"
# export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"

# NOTE: This too
# source ~/.git-prompt.sh

# if command -v pyenv 1>/dev/null 2>&1; then
#   eval "$(pyenv init -)"
# fi
