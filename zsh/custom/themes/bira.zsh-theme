# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"


if [[ $UID -eq 0 ]]; then
    local user_host='%{$terminfo[bold]$fg[red]%}%n@%m%{$reset_color%}'
    local user_symbol='#'
else
    local user_host='%{$terminfo[bold]$fg[green]%}%n@%m%{$reset_color%}'
    local user_symbol='$'
fi

local current_dir='%{$terminfo[bold]$fg[blue]%}%~%{$reset_color%}'
local rvm_ruby=''
if which rvm-prompt &> /dev/null; then
  rvm_ruby='%{$fg[red]%}‹$(rvm-prompt i v g)›%{$reset_color%}'
else
  if which rbenv &> /dev/null; then
    rvm_ruby='%{$fg[red]%}‹$(rbenv version | sed -e "s/ (set.*$//")›%{$reset_color%}'
  fi
fi

MODE_INDICATOR="──[%{$fg_bold[yellow]%}NORMAL%{$reset_color%}]"
RANGER_INDICATOR="──[%{$fg_bold[magenta]%}RANGER%{$reset_color%}]"

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

function ranger_prompt_info {
  if [[ "$RANGER_LEVEL" ]]; then
    echo "${RANGER_INDICATOR}"
  else
    echo ""
  fi
}

local git_branch='$(git_prompt_info)%{$reset_color%}'
local vi_prompt='$(vi_mode_prompt_info)%{$reset_color%}'
local ranger_prompt='$(ranger_prompt_info)%{$reset_color%}'

PROMPT="╭─[${user_host}]──[${current_dir}]${git_branch}${vi_prompt}${ranger_prompt}
╰─%B${user_symbol}%b "
#RPS1="%B${return_code}%b"

ZSH_THEME_GIT_PROMPT_PREFIX="──[%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}]"
ZSH_THEME_GIT_PROMPT_MODIFIED="M"
ZSH_THEME_GIT_PROMPT_UNTRACKED="U"
ZSH_THEME_GIT_PROMPT_ADDED="A"
ZSH_THEME_GIT_PROMPT_RENAMED="R"
ZSH_THEME_GIT_PROMPT_DELETED="D"
ZSH_THEME_GIT_PROMPT_STASHED="S"
ZSH_THEME_GIT_PROMPT_AHEAD="↑"
ZSH_THEME_GIT_PROMPT_BEHIND="↓"
ZSH_THEME_GIT_PROMPT_DIVERGED="↯"

