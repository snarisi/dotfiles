#  This should make it stop warning that the default is now zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

export PYENV_ROOT="$HOME/.pyenv/"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

if [[ -f ~/.bashrc ]]; then
	source ~/.bashrc
fi

if [[ -f `brew --prefix`/etc/bash_completion.d/brew ]]; then
    source `brew --prefix`/etc/bash_completion.d/brew
fi
