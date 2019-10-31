#!/usr/bin/env bash

# install a cmdstan release into a specified directory
#  - build binaries, compile example model to build model header
#  - symlink downloaded version as "cmdstan"

while getopts ":d:v:j:wrob:u:c" opt; do
  case $opt in
    d) RELDIR="$OPTARG"
    ;;
    v) VER="$OPTARG"
    ;;
    j) JOBS="$OPTARG"
    ;;
    w) WIN=1
    ;;
    r) REPO_CLONE=1
    ;;
    c) REPO_CHECKOUT_BRANCH=1
    ;;
    b) REPO_BRANCH="$OPTARG"
    ;;
    u) REPO_URL="$OPTARG"
    ;;
    o) OVERWRITE=1
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    ;;
  esac
done

if [ -z ${WIN} ]; then
    WIN=0
fi

if [ -z ${OVERWRITE} ]; then
    OVERWRITE=0
fi

if [ -z ${REPO_CLONE} ]; then
    REPO_CLONE=0
fi

if [ -z ${REPO_CHECKOUT_BRANCH} ]; then
    REPO_CHECKOUT_BRANCH=0
fi

if [ -z ${REPO_BRANCH} ]; then
    REPO_BRANCH="develop"
fi

if [ -z ${REPO_URL} ]; then
    REPO_URL="https://github.com/stan-dev/cmdstan.git"
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

build_cmdstan() {
    if [[ ${WIN} -ne 0 ]]; then
        mingw32-make -j${JOBS} build examples/bernoulli/bernoulli.exe
    else
        make -j${JOBS} build examples/bernoulli/bernoulli
    fi
}

INSTALL_DIR=cmdstan
pushd ${RELDIR} > /dev/null

if [[ ${REPO_CHECKOUT_BRANCH} -ne 0 ]]; then
    git fetch origin
    PULL_RC=$?
    if [[ ${PULL_RC} -ne 0 ]]; then
        echo "error pulling from the repository"
        echo "please check if the branch exists on the clone repository and"
        echo "there are not any unstashed changes in ${RELDIR}/cmdstan"
        exit ${GIT_RC}
    fi
    git checkout ${REPO_BRANCH}
    CHECKOUT_RC=$?
    if [[ ${CHECKOUT_RC} -ne 0 ]]; then
        echo "error checking out git branch"
        exit ${GIT_RC}
    fi
    git submodule update --recursive
    cleanup_cmdstan
    build_cmdstan
    echo "branch checkout and rebuild done"
    exit 0;
fi

if [[ ${OVERWRITE} -ne 1 ]]; then
    if [[ -d ${INSTALL_DIR} ]]; then
        echo "cmdstan found in ${RELDIR}/, installation stopped"
        echo "remove the cmdstan folder or set overwrite = TRUE in install_cmdstan()"
        echo ""
        echo "CmdStan installation location: `ls -Fd ${RELDIR}/cmdstan`"
        exit 0;
    fi
fi

if [[ -e cmdstan ]]; then
    echo "removing the existing installation of cmdstan"
    rm -rf cmdstan
fi

if [[ ${REPO_CLONE} -ne 0 ]]; then
    echo "cloning ${REPO_URL} and checking out branch ${REPO_BRANCH}"
    echo "this may take a few minutes ..."
    git clone --recursive ${REPO_URL} -b ${REPO_BRANCH} ${INSTALL_DIR}
    GIT_RC=$?
    if [[ ${GIT_RC} -ne 0 ]]; then
        echo "error cloning repository"
        exit ${GIT_RC}
    fi
else
    
    if [ -z ${VER} ]; then
        TAG=`curl -s https://api.github.com/repos/stan-dev/cmdstan/releases/latest | grep "tag_name"`
        echo $TAG > tmp-tag
        VER=`perl -p -e 's/"tag_name": "v//g; s/",//g' tmp-tag`
        rm tmp-tag
    fi

    CS=cmdstan-${VER}

    echo "installing cmdstan ${VER} in ${RELDIR}"
    echo "downloading ${CS}.tar.gz from github"

    curl -s -OL https://github.com/stan-dev/cmdstan/releases/download/v${VER}/${CS}.tar.gz
    CURL_RC=$?
    if [[ ${CURL_RC} -ne 0 ]]; then
        echo "github download failed, curl exited with: ${CURL_RC}"
        exit ${CURL_RC}
    fi
    echo "download complete"

    echo "unpacking archive"
    
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
