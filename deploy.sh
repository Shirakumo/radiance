#!/bin/bash                                                                           

readonly CWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly TARGET=${1-${CWD}}
readonly GIT_ROOT="git://github.com/Shirakumo/"
readonly CORE=("radiance-core" "radiance-drivers" "radiance-commons" "radiance-impls")
readonly MODS=("reader" "plaster" "chatlog" "purplish" "db-introspect" "manage-users")

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
