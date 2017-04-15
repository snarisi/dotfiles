if [ -f ~/.bashrc ]; then
	source ~/.bashrc
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
	. `brew --prefix`/etc/bash_completion
fi

if [ -f ~/.git-completion.bash ]; then
	. ~/.git-completion.bash
fi

# Turn on 256 color support...
if [ "x$TERM" = "xxterm"  ]; then
	export TERM="xterm-256color"
fi
export PATH="/usr/local/opt/redis@2.8/bin:$PATH"
