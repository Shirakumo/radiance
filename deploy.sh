#!/bin/bash                                                                           

readonly CWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly TARGET=${1-${CWD}}
readonly GIT_ROOT="git://github.com/Shinmera/"
readonly CORE=("radiance-core" "radiance-web")
readonly LIBS=("plump" "CLSS" "lquery" "clip" "staple" "piping" "verbose" "Universal-Config" "trivial-mimes" "trivial-indent" "crypto-shortcuts" "modularize" "modularize-hooks" "modularize-interfaces" "ratify")
readonly MODS=("radiance-reader" "radiance-plaster" "radiance-chatlog")

status(){
    echo -e "\e[32m\e[1m$@\e[0m"
}

clone_or_fetch(){
    if [ -d "$TARGET/$1/$2" ]; then
        status "> Updating $TARGET/$1/$2"
        cd $TARGET/$1/$2
        git pull
        cd $CWD
    else
        status "> Installing $GIT_ROOT$2.git"
        git clone $GIT_ROOT$2.git $TARGET/$1/$2
    fi
}

process_core(){
    status ">> Updating CORE"
    cd $TARGET
    git pull
    
    local i
    for i in "${CORE[@]}"; do
        clone_or_fetch "core" $i
    done
    status ""
}


process_libs(){
    status ">> Updating LIBS"
    local i
    for i in "${LIBS[@]}"; do
        clone_or_fetch "lib" $i
    done
    status ""
}


process_mods(){
    status ">> Updating MODS"
    local i
    for i in "${MODS[@]}"; do
        clone_or_fetch "modules" $i
    done
    status ""
}

main(){
    status "Deploying radiance to $TARGET\n"
    process_core
    process_libs
    process_mods
}

main
