if [[ -f ~/.bashrc ]]; then
	source ~/.bashrc
fi

if [[ -f `brew --prefix`/etc/bash_completion.d/brew ]]; then
    source `brew --prefix`/etc/bash_completion.d/brew
fi
