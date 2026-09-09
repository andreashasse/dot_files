# Workaround for elp/eqwalizer JAR disappearing from temp on macOS
export ELP_EQWALIZER_PATH="$HOME/.local/share/elp/eqwalizer.jar"


# Completions. Kördes tidigare av oh-my-zsh; utan den måste compinit köras
# explicit, annars failar compdef i completion-raderna längre ned.
autoload -Uz compinit
compinit

source ~/.zshenv

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"



[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=/Users/andreashasselberg/bin:$PATH
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
export PATH=${PATH}:`go env GOPATH`/bin

# Python (pip and pipx) installed programs
export PATH="/Users/andreashasselberg/.local/bin:$PATH"

# alias
alias cat="bat --theme=\"Visual Studio Dark+\""

# Aira Dev Ops
alias a_prod="export AWS_PROFILE=cloud-admin-prod && kubectx arn:aws:eks:eu-north-1:528895488893:cluster/prod"
alias a_systest="export AWS_PROFILE=cloud-admin-test && kubectx arn:aws:eks:eu-north-1:361629632765:cluster/systest"
alias a_uat="export AWS_PROFILE=cloud-admin-test && kubectx arn:aws:eks:eu-north-1:361629632765:cluster/uat"
alias a_tools="export AWS_PROFILE=cloud-admin-tools && kubectx arn:aws:eks:eu-north-1:660263384063:cluster/tools"
alias docker_login2="aws ecr get-login-password --region eu-north-1 | docker login --username AWS --password-stdin 660263384063.dkr.ecr.eu-north-1.amazonaws.com"
alias morning="a_tools && docker_login2"



alias a_iot_systest="export AWS_PROFILE=iot-dev-test && kubectx arn:aws:eks:eu-north-1:361629632765:cluster/systest"
alias a_iot_uat="export AWS_PROFILE=iot-dev-test && kubectx arn:aws:eks:eu-north-1:361629632765:cluster/uat"
alias a_iot_prod="export AWS_PROFILE=iot-dev-prod && kubectx arn:aws:eks:eu-north-1:528895488893:cluster/prod"

# setup z
eval "$(zoxide init zsh)"
source /Users/andreashasselberg/.config/broot/launcher/bash/br

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk
export EDITOR="zed"
export GIT_EDITOR="hx"

jwt-decode() {
  jq -R 'split(".") |.[0:2] | map(gsub("-"; "+") | gsub("_"; "/") | gsub("%3D"; "=") | @base64d) | map(fromjson)' <<< $1
}

function set_title_to_git_directory() {
  local git_dir
  git_dir=$(git rev-parse --show-toplevel 2>/dev/null)
  if [ $? -eq 0 ]; then
    # If current directory is the same as the Git root
    if [ "$PWD" = "$git_dir" ]; then
      echo -ne "\033]0;🗃️ $(basename "$git_dir")\007"
    else
      # If current directory is different, show git root and the relative path
      local current_dir="${PWD#$git_dir/}"
      echo -ne "\033]0;🗃️ $(basename "$git_dir") $current_dir\007"
    fi
  else
    # Set terminal title to current directory if not in a Git repository
    echo -ne "\033]0;$(basename "$PWD")\007"
  fi
}
# Hook into the prompt to update the title before each command prompt
# precmd_functions+=set_title_to_git_directory


rfv() (
  RELOAD='reload:rg --column --color=always --smart-case {q} || :'
  OPENER='if [[ $FZF_SELECT_COUNT -eq 0 ]]; then
            code -g {1}:{2}     # No selection. Open the current line in Vim.
          else
            code {+f}  # Build quickfix list for the selected items.
          fi'
  fzf --disabled --ansi --multi \
      --bind "start:$RELOAD" --bind "change:$RELOAD" \
      --bind "enter:become:$OPENER" \
      --bind "ctrl-o:execute:$OPENER" \
      --bind 'ctrl-/:toggle-preview' \
      --delimiter : \
      --preview 'bat --style=full --color=always --highlight-line {2} {1}' \
      --preview-window '~4,+{2}+4/3,<80(up)' \
      --query "$*"
)

docker_login() {
  aws ecr get-login-password --region eu-north-1 | docker login --username AWS --password-stdin 660263384063.dkr.ecr.eu-north-1.amazonaws.com
}

# Navi
export NAVI_PATH="$HOME/.config/navi/cheats:${NAVI_PATH}"
#export NAVI_FZF_OVERRIDES="--height=20 --no-select-1 --no-exit-0"
eval "$(navi widget zsh)"
#bindkey '^G' navi-widget
bindkey -M emacs '^G' _navi_widget

export PATH="$(brew --prefix ruby)/bin:$PATH"
export PATH="/opt/homebrew/lib/ruby/gems/3.4.0/bin:$PATH"

npm config set prefix ~/.npm-global
export PATH=~/.npm-global/bin:$PATH

# opencode
export PATH=/Users/andreashasselberg/.opencode/bin:$PATH

# bun completions
[ -s "/Users/andreashasselberg/.bun/_bun" ] && source "/Users/andreashasselberg/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
eval "$(mise activate zsh)"

# dexter completions
eval "$(dexter completion zsh)"

# task completions
eval "$(task --completion zsh)"
