local distro="$(. /etc/os-release; echo "$ID;$ID_LIKE")"

if [ "${MY_PROFILE_LOADED}" = "" ]; then
 local emulated="$(emulate)"
 emulate sh
 if [ -f ~/.xprofile ] && [ "$DISPLAY" != "" ]; then
   source ~/.xprofile
 elif [ -f ~/.profile ]; then
   source ~/.profile
 fi
 emulate "$emulated"
fi

export ADOTDIR="$HOME/.zsh/bundle"

source "$HOME/.zsh/zgen/zgen.zsh"

if ! zgen saved; then
  echo "Creating a zgen save"

  # Load the oh-my-zsh's library.
  zgen oh-my-zsh

  # Bundles from the default repo (robbyrussell's oh-my-zsh).
  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/autojump
  zgen oh-my-zsh plugins/cabal
  zgen oh-my-zsh plugins/dircycle
  zgen oh-my-zsh plugins/gitfast
  zgen oh-my-zsh plugins/mercurial

  # Syntax highlighting bundle.
  zgen load zsh-users/zsh-syntax-highlighting

  # Load the theme.
  zgen oh-my-zsh themes/robbyrussell

  zgen save
fi

# Aliases
alias vi="vim"
alias v="vim"
alias e="emacs"
alias g="git"
alias fn="find -name"

# Play nicely with Emacs's tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$'

# Disable right prompt
RPROMPT=""

alias a="ack --ignore-dir=tmp --ignore-dir=app/assets --ignore-dir=log --ignore-dir=assets"
