#!/usr/bin/env bash

# install a cmdstan release into a specified directory
#  - build binaries, compile example model to build model header
#  - symlink downloaded version as "cmdstan"

while getopts ":d:v:j:wbcf" opt; do
  case $opt in
    d) RELDIR="$OPTARG"
    ;;
    v) VER="$OPTARG"
    ;;
    j) JOBS="$OPTARG"
    ;;
    w) WIN=1
    ;;
    b) BACKUP_OLD=1
    ;;
    c) REPO_CLONE=1
    ;;
    f) FORCE=1
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    ;;
  esac
done

if [ -z ${WIN} ]; then
    WIN=0
fi

if [ -z ${BACKUP_OLD} ]; then
    BACKUP_OLD=0
fi

if [ -z ${REPO_CLONE} ]; then
    REPO_CLONE=0
fi

if [ -z ${FORCE} ]; then
    FORCE=0
fi

if [ -z ${R} ]; then
    R=0
fi

if [ -z ${JOBS} ]; then
    JOBS="2"
fi

if [ -z ${RELDIR} ]; then
    RELDIR="$HOME/.cmdstanr"
fi

if [[ ! -e ${RELDIR} ]]; then
   mkdir -p ${RELDIR}
fi

cleanup_cmdstan() {
    if [[ ${WIN} -ne 0 ]]; then
        mingw32-make clean-all
    else
        make clean-all
    fi
}

backup_cmdstan() {
    if [[ -e cmdstan ]]; then
        echo "backing up and cleaning installed cmdstan"
        pushd cmdstan > /dev/null
        # cleaning the backup
        cleanup_cmdstan
        pushd ../ > /dev/null
        # removing existing backup folder
        if [[ -e cmdstan_backup ]]; then
            rm -rf cmdstan_backup
        fi
        mv cmdstan/ cmdstan_backup/
    fi
}

build_cmdstan() {
    if [[ ${WIN} -ne 0 ]]; then
        mingw32-make -j${JOBS} build examples/bernoulli/bernoulli.exe
    else
        make -j${JOBS} build examples/bernoulli/bernoulli
    fi
}

echo "cmdstan dir: ${RELDIR}"

if [[ ${REPO_CLONE} -ne 0 ]]; then
    pushd ${RELDIR} > /dev/null
    if [[ ${BACKUP_OLD} -ne 0 ]]; then
        backup_cmdstan
    fi
    git clone --recursive https://github.com/stan-dev/cmdstan.git --single-branch -b develop cmdstan
else
    if [ -z ${VER} ]; then
        TAG=`curl -s https://api.github.com/repos/stan-dev/cmdstan/releases/latest | grep "tag_name"`
        echo $TAG > tmp-tag
        VER=`perl -p -e 's/"tag_name": "v//g; s/",//g' tmp-tag`
        rm tmp-tag
    fi

    CS=cmdstan-${VER}
    INSTALL_DIR=cmdstan
    echo "cmdstan version: ${VER}"

    pushd ${RELDIR} > /dev/null
    #testing if there are files in the cmdstan folder
    if [[ -d ${INSTALL_DIR} && -f ${INSTALL_DIR}/makefile ]]; then
        echo "cmdstan already installed, checking version"
        OLD_VER=$(grep '^CMDSTAN_VERSION := ' ${INSTALL_DIR}/makefile | sed -e 's/CMDSTAN_VERSION := //g')
        if [[ ! $VER > $OLD_VER && $FORCE -ne 1 ]]; then
            echo "installed cmdstan is the latest version"
            echo "only running clean and rebuild"
            pushd cmdstan > /dev/null
            echo "rebuilding cmdstan binaries"
            cleanup_cmdstan
            build_cmdstan
            exit 0;
        else
            if [[ ${BACKUP_OLD} -ne 0 ]]; then
                backup_cmdstan
            fi
        fi
    fi
    echo "installing ${CS}"

    echo "downloading ${CS}.tar.gz from github"
    curl -s -OL https://github.com/stan-dev/cmdstan/releases/download/v${VER}/${CS}.tar.gz
    CURL_RC=$?
    if [[ ${CURL_RC} -ne 0 ]]; then
        echo "github download failed, curl exited with: ${CURL_RC}"
        exit ${CURL_RC}
    fi
    echo "download complete"

    echo "unpacking archive"
    if [[ -e cmdstan ]]; then
        rm -rf cmdstan
    fi

    mkdir cmdstan
    tar xzf ${CS}.tar.gz -C cmdstan/ --strip-components 1
    TAR_RC=$?
    if [[ ${TAR_RC} -ne 0 ]]; then
        echo "corrupt download file ${CS}.tar.gz, tar exited with: ${TAR_RC}"
        exit ${TAR_RC}
    fi
    rm ${CS}.tar.gz
fi

pushd cmdstan > /dev/null
echo "building cmdstan binaries"
build_cmdstan
echo "installed ${CS} to ${RELDIR}/cmdstan"

# cleanup
pushd -0 > /dev/null
dirs -c > /dev/null
echo ""
echo "CmdStan installation location: `ls -Fd ${RELDIR}/cmdstan`"
