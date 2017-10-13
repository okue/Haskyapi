# Haskyapi Command Support

_haskyapi()
{
  local first="runserver migrate --help -h --version -v"
  local server="--root -r --port -p --ip -i --help -h"
  local migrate="--help -h"
  local cur=${COMP_WORDS[COMP_CWORD]}
  local prev=${COMP_WORDS[COMP_CWORD-1]} # previous argument
  case "$COMP_CWORD" in
    1) COMPREPLY=( $(compgen -W "$first" -- $cur) ) ;;
    *)
      case $prev in
        runserver)
          COMPREPLY=( $(compgen -W "$server"  -- $cur) ) ;;
        migrate)
          COMPREPLY=( $(compgen -W "$migrate" -- $cur) ) ;;
        *)
          case ${COMP_WORDS[1]} in
            runserver)
              COMPREPLY=( $(compgen -W "$server  $(ls)" -- $cur) ) ;;
            migrate)
              COMPREPLY=( $(compgen -W "$migrate $(ls)" -- $cur) ) ;;
          esac
      esac
  esac
}

complete -o default -F _haskyapi haskyapi

