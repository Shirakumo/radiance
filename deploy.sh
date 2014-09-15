#!/bin/bash                                                                           

readonly CWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly TARGET=${1-${CWD}}
readonly GIT_ROOT="https://github.com/Shinmera/"
readonly CORE=("radiance-core" "radiance-web")
readonly LIBS=("plump" "CLSS" "lquery" "clip" "staple" "piping" "verbose" "Universal-Config" "trivial-mimes" "trivial-indent" "clip" "crypto-shortcuts" "modularize" "modularize-hooks" "modularize-interfaces" "ratify")
readonly MODS=("radiance-reader" "radiance-plaster" "radiance-chatlog")

clone_or_fetch(){
    if [ -d "$TARGET/$1/$2" ]; then
        echo -e "Updating $TARGET/$1/$2"
        cd $TARGET/$1/$2
        git pull
        cd $CWD
    else
        echo -e "Installing $TARGET/$1/$2"
        git clone $GIT_ROOT$1 $TARGET/$1/$2
    fi
}

process_core(){
    echo -e "Updating RADIANCE"
    cd $TARGET
    git pull
    
    local i
    for i in "${CORE[@]}"; do
        clone_or_fetch "core" $i
    done
}


process_libs(){
    local i
    for i in "${LIBS[@]}"; do
        clone_or_fetch "lib" $i
    done
}


process_mods(){
    local i
    for i in "${MODS[@]}"; do
        clone_or_fetch "modules" $i
    done
}

main(){
    echo -e "Deploying radiance to $TARGET\n"
    process_core
    process_libs
    process_mods
}

main
