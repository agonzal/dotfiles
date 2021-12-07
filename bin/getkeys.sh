#!/bin/sh 

DWM_DIR=~/repos/dwm 
FILE=~/repos/dwm/config.def.h 

helpmenu() {
    echo rice - a tool to create a markdown file from dwm config
    echo 
    echo Usage: rice \[filename\] \[options\]
    echo
    echo Filename: 
    echo "  The filename of your config, must end in .h"
    echo
    echo Options: 
    echo "  markdown - to generate a markdown file."
    echo "  status   - to give some status about your bindings from your config."
    echo "  key      - to see if a key is available to map, if not will display the maps you already have."
}

# Get helper script
# shellcheck disable=SC1090,SC1091

CLEAN="cleanup"
checkdwm(){
    [ ! -f "$FILE" ] && printf "File '$FILE' is invalid!"
    case $1 in 
        markdown | md) mddwm  "$2" ;;
        status   | s)  statdwm ;;
        key      | k)  keydwm "$2" ;;
        *) printf "You must provide an argument for the file! Type 'rice help' to get more information!"
    esac
}

getkeys() { grep -oP '^\s*\{\s*[a-zA-Z0].*\}\,' "$1" | grep -v 'NULL\|Clk\| KEY,' > "$2" ; }

printkeys() {
    echo "$1" | sort | uniq -c | while IFS= read -r l; do
        l="${l#"${l%%[![:space:]]*}"}"
        [ "${l%% *}" = "1" ] && TIME="time " || TIME="times"
        echo "${l%% *}" "$TIME" "$(echo "${l##* }" | sed 's/|/ + /g')"
    done | column -x
}

mddwm(){
    # select the relevant keys
    getkeys "$FILE" "$CLEAN"
    [ -s "$CLEAN" ] || printf "No keys found!"
    # clean up
    sed -i -r 's/(^\s*\{\s*)|((\"\)|\s*\})?\s*\}\,?\s*$)|(Mask|KEY|MOD,)|([A-Z0-9]*XK_)|(SHCMD\(\"?)|((\s\"))|(\{0\})|(\{\s*\.[iufv]+\s*=\s*&?)//g; s/\s\s*/ /g; s/MOD/Super/g; s/ALT/Alt/g; s/\|/ + /g; s/^0\,\s*//g' "$CLEAN"
    # get commands
    grep -oP '\w+cmd' "$CLEAN" | while read -r line ; do
        [ -z "$line" ] && continue
        LN="$(grep "\*$line" "$FILE")"
        VALUE="$(echo "$LN" | grep -oP '\{\s*\".*\}\;')"
        REPLACE="$(echo "$VALUE" | sed -r 's/(\"|,|\s|;|NULL|\}|\{)+/ /g')"
        sed -i "s|$line|${REPLACE}|g" "$CLEAN"
    done
    # get layouts
    LAYOUTS="layouts"
    grep -oP '^\s*\{\s*\".*\"\,\s*[a-zNULL]+\s*\}\,' "$FILE" > "$LAYOUTS"
    # replace layouts with current number
    grep -oP 'layouts[[0-9]+]' "$CLEAN" | while read -r l; do
        [ -z "$l" ] && continue
        NUM="$(echo "$l" | sed -r 's/[^0-9]*//g')"
        NUM="$(( NUM + 1 ))"
        LINE="$(sed -n "$NUM"p "$LAYOUTS")"
        NAME="$(echo "$LINE" | grep -oP '[a-z]+\s+')"
        [ -z "$NAME" ] && NAME="float"
        # replace
        l="$(echo "$l" | sed 's/\[/\\[/g; s/\]/\\]/g')"
        sed -i "s/$l/${NAME}/g" "$CLEAN"
    done
    [ "$1" = "sort" ] && sort "$CLEAN" -o "$CLEAN"
    # Write to markdown file
    RESULT="result.md"
    echo "| Key | Function | Argument |" > "$RESULT"
    echo "| :-: | :-: | :- |" >> "$RESULT"
    while IFS= read -r l; do 
        NUM="$(echo "$l" | sed 's/[^,]//g')"
        case "${#NUM}" in 
            2) echo "| $l |" | sed 's/,/ |/; s/spawn,/spawns |/; s/,/ | /' >> "$RESULT" ;;
            3) echo "| $l |" | sed 's/,/ +/; s/,/ |/; s/spawn,/spawns |/; s/,/ | /' >> "$RESULT" ;;
        esac
    done < "$CLEAN"
    # remove unnecesarry files and display message
    rm "$CLEAN" "$LAYOUTS"
#    echo "Done! Markdown written in $RESULT"
    /bin/cat $RESULT
}

statdwm() {
    getkeys "$FILE" "$CLEAN"
    [ -s "$CLEAN" ] || printf "No keys found!"
    NCLEAN="newclean"
    # get the current keys
    grep -oP '\S\s[a-zA-Z0-9|_]+\,\s*[a-zA-Z0-9_,]+' "$CLEAN" > "$NCLEAN"
    # remove spaces and {
    sed -i -r 's/\{\s*|(KEY|Mask|[A-Z0-9]*XK_)//g; s/\s\s*/ /g; s/MOD/Super/g; s/ALT/Alt/g' "$NCLEAN"
    # display total bindings
    TOTAL="$(wc -l "$NCLEAN")"
    echo Total of bindings: "${TOTAL%% *}" 
    # display more information about the keys
    printf '\n%s\n' "You have mapped: "
    printkeys "$(awk -F ',' '{print $1}' "$NCLEAN" )"
    printf '\n%s\n' "with: "
    printkeys "$(awk -F ',' '{print $2}' "$NCLEAN" )"
    # remove files
    rm "$NCLEAN" "$CLEAN"
}

keydwm() {
    [ -z "$1" ] && printf "No key provided"
    # get relevant keys
    getkeys "$FILE" "$CLEAN"
    [ -s "$CLEAN" ] || printf "No keys found!"
    # get keys with input from the user
    MKEYS="matched_keys"
    grep "XK_$1," "$CLEAN" > "$MKEYS"
    [ -s "$MKEYS" ] || printf "You have no mapped keys!"
    # clean up
    sed -i -r 's/(^\s*\{\s*)|([a-zA-Z0-9]+_)|(KEY|Mask)|\s*\}\,?$//g; s/\s\s*/ /g; s/MOD/Super/g; s/\|/ + /g' "$MKEYS"
    # display keys
    column -s "," -t "$MKEYS"
    # remove files
    rm "$MKEYS" "$CLEAN"
}

FILE="$1"
case $1 in
    help | h) helpmenu ;;
    *.h) checkdwm "$2" "$3" "$4";;
    *) printf "File '$1' is invalid" ;;
esac
