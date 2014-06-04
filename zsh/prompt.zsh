autoload colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

if (( $+commands[git] ))
then
  git="$commands[git]"
else
  git="/usr/bin/git"
fi

git_branch() {
  echo $($git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
  if $(! $git status -s &> /dev/null)
  then
    echo ""
  else
    #if [[ $($git status --porcelain) == "" ]]
    if [[ $(unpushed) == "" ]]
    then
      echo "%{$FG[032]%}(%{$reset_color%}%{$FX[bold]%}%{$FG[075]%}$(git_prompt_info)%{$reset_color%}%{$FG[032]%})%{$reset_color%}"
    else
      echo "%{$FG[032]%}(%{$reset_color%}%{$FX[bold]%}%{$FG[214]%}$(git_prompt_info)%{$reset_color%}%{$FG[032]%})%{$reset_color%}"
    fi
  fi
}

git_prompt_info () {
 ref=$($git symbolic-ref HEAD 2>/dev/null) || return
# echo "(%{\e[0;33m%}${ref#refs/heads/}%{\e[0m%})"
echo "${ref#refs/heads/}$(uncomitted_changes)"
}

unpushed () {
  $git cherry -v @{upstream} 2>/dev/null
}

need_push () {
  if [[ $(unpushed) == "" ]]
  then
    echo ""
  else
    echo "%{$FX[bold]%}%{$FG[214]%}!%{$reset_color%}"
  fi
}

uncomitted_changes() {
  if ! git diff-files --quiet --ignore-submodules
  then
    echo "%{$FX[bold]%}%{$FG[214]%}*%{$reset_color%}"
  else
    echo ""
  fi
}

ruby_version() {
  if (( $+commands[rbenv] ))
  then
    echo "$(rbenv version | awk '{print $1}')"
  fi

  if (( $+commands[rvm-prompt] ))
  then
    echo "$(rvm-prompt | awk '{print $1}')"
  fi
}

rb_prompt() {
  if ! [[ -z "$(ruby_version)" ]]
  then
    echo "%{$FX[bold]%}%{$FG[075]%}$(ruby_version)%{$reset_color%} "
  else
    echo ""
  fi
}

directory_name() {
  echo "%{$FX[bold]%}%{$FG[032]%}%1/%{$reset_color%}"
}

export PROMPT=$'\n$(rb_prompt)in $(directory_name)$(git_dirty)› '
set_prompt () {
  export RPROMPT="%{$fg_bold[cyan]%}%{$reset_color%}"
}

precmd() {
  title "zsh" "%m" "%55<...<%~"
  set_prompt
}
