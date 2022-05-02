#!/bin/sh

set -e

usage=
newline=cat
file=

while [ $# -gt 0 ]; do
    case "$1" in
        --h)
            usage=1
            ;;
        --command)
            # Configuration of the command
            #
            # modes   - CLI mode (oper config)
            # styles  - CLI style (c i j)
            # cmdpath - Full CLI command path
            # help    - Command help text
            #
            # Configuration of the parameters
            #
            # name     - (optional) name of the parameter
            # presence - optional or mandatory
            # type     - void - A parameter without a value
            # words    - any - Multi word param. Only valid for the last param
            # flag     - Extra word added before the parameter value
            # prefix   - Extra string prepended to the parameter value
            # help     - Command help text
            cat << EOF
begin command
  modes: config
  styles: c i j
  cmdpath: save-device-cfg
  help: save device settings
  more: true
end

begin param
 presence: mandatory
 flag: -f
 help: File to save to
end

EOF
            exit
            ;;
        -f)
            # file name
            file=$2
            shift
            ;;
        *)
            break
            ;;
    esac
    shift
done

ncs-maapi --clicmd "save $file devices device * | de-select config | nomore"
