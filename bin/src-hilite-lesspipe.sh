#! /bin/bash
#
# Based on http://git.savannah.gnu.org/cgit/src-highlite.git/tree/src/src-hilite-lesspipe.sh.in
# by Lorenzo Bettini
#
# Modified by Jay Caines-Gooby to support piped files
# jay@gooby.org
# @jaygooby
#
# Typically called by setting:
#
# export LESSOPEN="|-/path/to/src-hilite-lesspipe.sh %s"
# export LESS=-R
#
# If we're less-ing a file, %s will be replaced by the name of the file. If
# there's no file and we're reading from a pipe, then %s is set to -
#
# This script differs from the original src-hilite-lesspipe.sh
# in that it can handle pipes and files with no extensions and will
# attempt to guess their language using the file command.
#
# So as well as invoking on regular files:
#
# less some.rb
# less some.py
#
# It will should be able to work on:
#
# less no-extension-but-contains-perl
#
# and even with more complex examples (my original motivation
# https://unix.stackexchange.com/questions/469982/how-can-i-use-source-highlight-with-git-show)
#
# git show master:some.rb
#
# It uses bashisms to do this, so is no longer a pure POSIX sh script.
set -eu

# Users can override the guessed language by setting SRCLANG:
# SRCLANG=c git show master:app/views/layouts/application.html.erb
SRCLANG=${SRCLANG:-}

guess_language() {
  lang=$(echo -e ${1:-} | file - | cut -d" " -f2)
  echo $(tr [A-Z] [a-z] <<< "$lang")
}

# check if the language passed as $1 is known to source-highlight
# In an earlier version of this script I set a fallback (c.lang)
# but this causes issues with paging man pages etc
check_language_is_known() {
  fallback=""
  lang=$(source-highlight --lang-list | cut -d' ' -f1 | grep "${1:-}" || true)
  lang=${lang:-$fallback}
  echo $lang
}

for source in "$@"; do
  case $source in
  	*ChangeLog|*changelog)
      source-highlight --failsafe -f esc --lang-def=changelog.lang --style-file=esc.style -i "$source" ;;
  	*Makefile|*makefile)
      source-highlight --failsafe -f esc --lang-def=makefile.lang --style-file=esc.style -i "$source" ;;
  	*.tar|*.tgz|*.gz|*.bz2|*.xz)
      lesspipe "$source" ;;
    *)

      # naive check for a file extension; let source-highlight infer language
      # but only when source isn't - (ie. from a piped file)
      if [[ "$source" != "-" && $(basename "$source") =~ \. ]]; then
        source-highlight --failsafe --infer-lang -f esc --style-file=esc.style -i "$source"
      else
        # We're being piped to, or the filename doesn't have an extension
        # so guess the language.

        # When we're being piped to, we cat stdin, but when it's a file
        # without an extension, we cat the file instead.

        # unset IFS so line breaks are preserved and capture the file's contents
        # (will only work for files up to bash's available memory). There should
        # be a better way to replicate this with tee or process substitution...
        IFS= file=$([ "source" = "-" ] && cat || cat "$source")
        lang=$(guess_language $file)
        lang=$(check_language_is_known $lang)

        # Don't call if source-highlight doesn't know the language
        # BUT also let users override the guessed lang if the environment
        # variable SRCLANG is set. This can help where you know e.g. your
        # source code is c-like, but source-highlight has no specific syntax
        # definition for your code
        #
        # SRCLANG=c git show master:app/views/layouts/application.html.erb

        [ -n "$SRCLANG" ] && lang="$SRCLANG"

        if [ -n "$lang" ]; then
          echo $file | source-highlight --failsafe -f esc --src-lang=$lang --style-file=esc.style
        else
          echo $file
        fi
      fi

      ;;
  esac
done
