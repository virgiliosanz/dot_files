
# # Load completions
# autoload -Uz compinit && compinit

# # Keybindings
# bindkey -e
# bindkey '^p' history-search-backward
# bindkey '^n' history-search-forward
# bindkey '^[w' kill-region

source ~/.env.secrets

# # Aliases
source ~/.aliases

# # Shell integrations
eval "$(fzf --zsh)"


# ################
# # Some env vars
export EDITOR=vim


# ## MySQL
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/mysql-client/lib"
export CPPFLAGS="-I/opt/homebrew/opt/mysql-client/include"

# For pkg-config to find mysql-client you may need to set:
export PKG_CONFIG_PATH="/opt/homebrew/opt/mysql-client/lib/pkgconfig"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

if [[ -f "/opt/homebrew/bin/brew" ]]; then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
source "$(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh"


# ## Google cloud
# # bug in google-cloud-sdk package
source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"

# Starship
eval "$(starship init zsh)"

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
