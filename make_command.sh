#!/usr/bin/env bash

EMACS=emacs

function command.package-install {
  # http://hacks-galore.org/aleix/blog/archives/2013/01/08/install-emacs-packages-from-command-line
  # 上で書かれている内容に加えて (package-initialize) も実行する必要がある様だ。

  # --eval に全部指定しても期待通りに動かない
  # -l にプロセス置換を指定しても期待通りに動かない
  {
    echo "(require 'package)"
    echo "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.milkbox.net/packages/\") t)"
    echo "(setq url-http-attempt-keepalives nil)"
    echo "(package-refresh-contents)"
    echo "(package-initialize)"
    while (($#)); do
      echo "(package-install '$1)"
      shift
    done
    echo "(message \"done\")"
  } > tmp.el

  $EMACS --batch -l tmp.el; local ext=$?
  rm tmp.el
  return "$ext"
}

if (($#==0)); then
  {
    echo "usage: make_command.sh <name> [args...]"
    echo
    echo "NAME:"
    declare -F | awk '/^declare -f command./{sub(/^declare -f command./,"  ");print}'
    echo
  } >&2
  exit 1
fi

alpha=$1
shift
if ! declare -f "command.$alpha" &>/dev/null; then
  printf "make_command: %q is not valid command name.\n" "$alpha" >&2
fi

"command.$alpha" "$@"
