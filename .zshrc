# Workaround för att elp/eqwalizer-JAR försvinner ur temp på macOS
export ELP_EQWALIZER_PATH="$HOME/.local/share/elp/eqwalizer.jar"

# --- mise: verktyg och språkversioner -------------------------------------
# Måste ligga först, resten nedan förutsätter att verktygen finns i PATH.
# Varje verktyg nedan är villkorat, så att filen laddar rent på en maskin
# där ingenting är installerat än.
if (( $+commands[mise] )); then
  eval "$(mise activate zsh)"
fi

# --- fzf: fuzzy-sökning ---------------------------------------------------
# ctrl+t väljer fil, alt+c hoppar till katalog, ** + tab kompletterar sökvägar.
# fd som källa gör sökningen snabb och respekterar .gitignore.
if (( $+commands[fd] )); then
  export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND='fd --type d --hidden --exclude .git'
fi
if (( $+commands[fzf] )); then
  source <(fzf --zsh)
fi

# --- zoxide: smartare cd --------------------------------------------------
# z <del av sökväg> hoppar till kataloger du besökt ofta, zi väljer interaktivt.
if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

# --- atuin: historik ------------------------------------------------------
# Sist av alla, så att atuins ctrl+r vinner över fzf:s variant av samma tangent.
if (( $+commands[atuin] )); then
  eval "$(atuin init zsh)"
fi

# --- prompt ---------------------------------------------------------------
# Kort prompt: katalog, sedan en grå upplysning, sedan en pil som blir röd
# vid fel. Upplysningen beror på var du står:
#   länkad worktree -> repots namn, och katalogen kortas till ett led, eftersom
#                      worktree-mappen ändå inte säger vilket repo det gäller
#   vanligt repo    -> grennamnet, utan ägarprefix och avklippt om det är långt,
#                      dolt helt när det bara upprepar katalognamnet
#   utanför git     -> ingenting
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '%b'

_prompt_path='%2~'
_prompt_info=''
_prompt_git() {
  vcs_info
  local branch=${vcs_info_msg_0_}
  _prompt_path='%2~'
  _prompt_info=''
  [[ -z $branch ]] && return

  # Ett enda git-anrop avgör om det här är en länkad worktree: då skiljer sig
  # katalogen för worktreen från repots gemensamma .git-katalog.
  local -a gitinfo
  gitinfo=(${(f)"$(git rev-parse --git-dir --git-common-dir --show-toplevel 2>/dev/null)"})
  if [[ ${#gitinfo} -eq 3 && ${gitinfo[1]:A} != ${gitinfo[2]:A} ]]; then
    # Ett led räcker när du står i worktreens rot, annars behövs två för att
    # inte tappa bort var i trädet du är.
    [[ ${PWD:A} == ${gitinfo[3]:A} ]] && _prompt_path='%1~'
    _prompt_info=" %F{242}${${gitinfo[2]:A:h}:t}%f"
    return
  fi

  branch=${branch##*/}
  [[ $branch == ${PWD:t} ]] && return
  (( ${#branch} > 24 )) && branch="${branch[1,23]}…"
  _prompt_info=" %F{242}${branch}%f"
}
precmd_functions+=(_prompt_git)

setopt prompt_subst
PROMPT='%F{blue}${_prompt_path}%f${_prompt_info} %(?.%F{green}.%F{red})❯%f '

# --- completions ----------------------------------------------------------
# Alla completion-kataloger samlade på ett ställe, compinit körs en enda gång
# efter att fpath är komplett. Docker Desktop lägger gärna tillbaka ett eget
# block sist i filen vid uppdatering; dess katalog finns redan med här, så det
# blocket kan då tas bort igen.
fpath=(
  ~/.zsh/completions
  ~/.docker/completions
  /opt/homebrew/share/zsh/site-functions
  $fpath
)
autoload -Uz compinit
compinit

# --- alias ----------------------------------------------------------------
# bat är cat med syntaxfärger och radnummer. --paging=never gör att korta
# filer skrivs rakt ut i stället för att öppnas i en pager.
# Behöver du riktiga cat, skriv "command cat" eller "\cat".
if (( $+commands[bat] )); then
  alias cat='bat --paging=never'
fi

# --- editor ---------------------------------------------------------------
# --wait gör att verktyg som väntar in redigeringen fungerar, i stället för
# att fortsätta direkt när Zed-fönstret öppnats. Git använder vim, satt i
# ~/.gitconfig, och påverkas inte av detta.
export EDITOR='zed --wait'
export VISUAL="$EDITOR"
