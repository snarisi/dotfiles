if [ -f ~/.bashrc ]; then
	source ~/.bashrc
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
	. `brew --prefix`/etc/bash_completion
fi

if [ -f ~/.git-completion.bash ]; then
	. ~/.git-completion.bash
fi

 source ~/.git-prompt.sh
# export PS1="➜ \e[1;36m\]\W \e[0;37m\e[1;31m\$(__git_ps1)\] \e[0;37m\]"

function parse_git_dirty {
	[[ $(git status --porcelain) != "" ]] && echo "*"
}
function parse_git_branch {
	repo_info="$(git rev-parse --git-dir --is-inside-git-dir \
				--is-bare-repository --is-inside-work-tree \
						--short HEAD 2>/dev/null)"
	
	if [ -z "$repo_info" ]; then
		echo ""
	else
		git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
	fi	
}

function dirty {
	repo_info="$(git rev-parse --git-dir --is-inside-git-dir \
				--is-bare-repository --is-inside-work-tree \
						--short HEAD 2>/dev/null)"

	if [ -z "$repo_info" ]; then
		echo ""
	else
		[[ $(git status --porcelain) != "" ]] && echo "*"
	fi	
}

PS1='\W$(__git_ps1 "\[\e[32m\] [%s]\[\e[0m\]") '
# PS1='\W$(__git_ps1 "\[\e[32m\] [%s$(dirty)]\[\e[0m\]")$ '
export PATH=/usr/local/bin:/usr/local/share/python:/usr/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/nand2tetris/tools
alias nand2tetris=HardwareSimulator.sh
alias autotrader_stage='workon autotrader && source ~/.autotrader_stage && env | grep SQL && env | grep PROXY'

export PROJECT_HOME=/Users/samnarisi/Development

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"
export PATH

PATH=/opt/local/bin:$PATH
