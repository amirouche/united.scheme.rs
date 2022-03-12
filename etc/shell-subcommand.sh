#!/usr/bin/env bash
#
# Copyright (c) 2018 rcmdnk
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
## Comments:
#
# Copied from:
#
#   https://github.com/rcmdnk/shell-subcommand/tree/ccfab23cfd66dcb8d5aeedb9de2e8aa53bf78c12
#
# TODO: Make it portable to POSIX shell
#
## Documentation
#
# Template for shell command with subcommand.
#
# Subcommand can be defined by alias, function, or executable in PATH,
# like Git command.
#
# ## Installation
#
# Put **bin/shell-subcommand** file where as you like.
#
# ## Getting started
#
# Prepare main script, e.g. `mycmd`.
#
# The minimum script is:
#
# ```mycmd
# #!/usr/bin/env bash
#
# source /path/to/shell-subcommand
# ```
#
# Then, you can call executables in the PATH
# which have a name starting with `mycmd-`.
#
# e.g., if you have an executable named `mycmd-exe`,
# you can call `mycmd exe`
#
# ## Use alias/function in your environment
#
# `shell-subcommand` can find alias/function,
# but alias/function are not passed to the script.
#
# If they are defined in **.bashrc**, just call them before `shell-subcommand`.
#
# ```mycmd
# #!/usr/bin/env bash
#
# source ~/.bashrc
# source /path/to/shell-subcommand
# ```
#
# Of course, you can defined them in the script, too:
#
# ```mycmd
# #!/usr/bin/env bash
#
# alias mycmd-hello="echo Hello world!"
# mycmd-hello-func () {
#   echo "Hello, this is function!"
# }
# source /path/to/shell-subcommand
# ```
#
# Check help:
#
#     $ mycmd help
#     usage: mycmd <sub-commands> [options]
#
#     sub commands: hello hello-func help
#
# Now you can call
#
#     $ mycmd hello
#     $ mycmd hello-func
#
# ## Help description
#
# You can give a short description for the help:
#
#     SHORT_DESCRIPTION="My command with various sub commands!"
#
# Then, it shows
#
#     $ mycmd
#     mycmd: My command with various sub commands!
#
#     usage: mycmd <sub-commands> [options]
#
#     sub commands: hello hello-func help
#
# If you want to add more help, you can change `usage...` sentences by `HELP_MAIN`, like
#
#     HELP_MAIN="usage: $(basename "$0") <sub-commands> [options]
#
#     sub-commands are made from alias, function, and executables in PATH."
#
# Now it shows:
#
#     $ mycmd
#     mycmd: My command with various sub commands!
#
#     usage: mycmd <sub-commands> [options]
#
#     sub-commands are made from alias, function, and executables in PATH.
#
#
#     sub commands: hello hello-func help
#
# ## Example
#
# Check example: [mycmd](https://github.com/rcmdnk/shell-subcommand/blob/master/example/mycmd).

MAIN_CMD=${MAIN_CMD:-$(basename "$0")}
PREFIX="${MAIN_CMD}-"

SHORT_DESCRIPTION=${SHORT_DESCRIPTION:-""}
HELP_MAIN=${HELP_MAIN:-"usage: $MAIN_CMD <sub-commands> [options]"}

if [ -n "$SHORT_DESCRIPTION" ];then
HELP=${HELP:-"$MAIN_CMD: $SHORT_DESCRIPTION

$HELP_MAIN"}
else
HELP=${HELP:-"$HELP_MAIN"}
fi

print_help () {
  cat << EOF
$HELP

sub commands: $commands
EOF
}

print_invalid () {
  cat << EOF
Invalid sub command: $1

Sub commands: $commands
EOF
}

get_alias () {
  for cmd in $(alias|grep "alias $PREFIX"|cut -d' ' -f2|cut -d= -f1);do
    c=${cmd/$PREFIX/}
    commands="$commands $c"
  done
}

get_function () {
  for cmd in $(declare -F|grep "declare -f $PREFIX"|cut -d' ' -f3);do
    c=${cmd/$PREFIX/}
    commands="$commands $c"
  done
}

get_exe () {
  local orig_ifs=$IFS
  IFS=":"
  local pathes=($PATH)
  IFS=$orig_ifs
  local p
  local f
  local cmd
  for p in "${pathes[@]}";do
    if [ ! -d "$p" ];then
      continue
    fi
    while read -r f;do
      if [ -x "$f" ];then
        cmd="${f##*/}"
        c=${cmd/$PREFIX/}
        commands="${commands} $c"
      fi
    done < <(find  "$p" -maxdepth 1 -name  "${PREFIX}*")
  done
}

get_commands () {
  commands=""
  get_alias
  get_function
  get_exe
  commands="$(for c in $commands;do echo $c;done |sort -u|tr $'\n' ' ')help"
}

check_command () {
  get_commands
  if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "help" ];then
    print_help
    exit
  fi
  cmd="$1"
  shift
  if ! echo " $commands "|grep -q " $cmd ";then
    print_invalid "$cmd"
    exit 1
  fi
}

execute () {
  check_command "$@"
  shift
  "$PREFIX$cmd" "$@"
}

execute "$@"
