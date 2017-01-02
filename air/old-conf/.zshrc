# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt autocd notify
unsetopt appendhistory extendedglob nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/aske/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

#autoload -U promptinit
#promptinit
#prompt redhat

alias ls='ls --color=auto'
alias ll='ls --color=auto -lh'
alias la='ls --color=auto -lha'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias zgrep='zgrep --color=auto'
alias e='emacs -nw'
alias ec='emacs ~/.emacs'
alias v='vim'
alias s='yaourt -Ss'
alias i='yaourt -S'
alias u='yaourt -Suya'
alias q='yaourt -Qs'
alias r='yaourt -R'

alias a='ack'
alias g='git'
alias ga='git add'
alias gc='git commit'
alias gs='git status'
alias gd='git diff'
alias gcl='git clone'
alias gp='git push'
alias gpl='git pull'

export EDITOR="emacs -nw"
export PATH="${PATH}:/usr/cuda/bin"

export PYTHONDOCS=/usr/share/doc/python/html/                                        
export PATH="${PATH}:/opt/vx32/bin"    
export PATH=~/.cabal/bin:$PATH
export PATH=~/.gem/ruby/1.9.1/bin:$PATH
export PATH=~/.gem/ruby/2.0.0/bin:$PATH
export _JAVA_AWT_WM_NONREPARENTING=1
export RSENSE_HOME=/opt/rsense/

source /usr/share/zsh/scripts/antigen/antigen.zsh
# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle archlinux
antigen bundle autojump
antigen bundle dircycle

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme robbyrussell

# Tell antigen that you're done
antigen apply
