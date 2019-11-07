#!/usr/bin/env bash

# install CmdStan into a specified directory
#  - build binaries, compile example model to build model header

while getopts ":d:v:j:wrob:u:cp" opt; do
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
    p) CLEAN_AND_REBUILD=1
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    ;;
  esac
done

if [ -z ${WIN} ]; then
    WIN=0
fi

if [ -z ${CLEAN_AND_REBUILD} ]; then
    CLEAN_AND_REBUILD=0
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
        mingw32-make -j${JOBS} build
    else
        make -j${JOBS} build
    fi
}

build_example() {
    if [[ ${WIN} -ne 0 ]]; then
        mingw32-make -j${JOBS} examples/bernoulli/bernoulli.exe
    else
        make -j${JOBS} examples/bernoulli/bernoulli
    fi
}

error_msg() {
  printf "%s\n" "$*" >&2;
}

INSTALL_DIR=cmdstan
pushd ${RELDIR} > /dev/null

if [[ ${REPO_CHECKOUT_BRANCH} -ne 0 ]]; then
    git fetch origin
    PULL_RC=$?
    if [[ ${PULL_RC} -ne 0 ]]; then
        error_msg "Error: pulling from the repository failed."
        error_msg "Please check that the branch exists in the repository to be cloned,"
        error_msg "and that there are not any unstashed changes in ${RELDIR}/cmdstan."
        exit ${GIT_RC}
    fi
    git checkout ${REPO_BRANCH}
    CHECKOUT_RC=$?
    if [[ ${CHECKOUT_RC} -ne 0 ]]; then
        error_msg "Error: checking out git branch failed."
        exit ${GIT_RC}
    fi
    git submodule update --recursive
    if [[ ${CLEAN_AND_REBUILD} -ne 0 ]]; then
        cleanup_cmdstan
        build_cmdstan
        echo "* Finished checking out branch and rebuilding."
    else
        echo "* Finished checking out branch."
    fi
    exit 0;
fi

if [[ ${OVERWRITE} -ne 1 ]]; then
    if [[ -d ${INSTALL_DIR} ]]; then
        error_msg "Error: an installation already exists at ${RELDIR}/cmdstan"
        error_msg "Please remove or rename the 'cmdstan' folder or set overwrite=TRUE in install_cmdstan()."
        exit 0; # don't actually error though
    fi
fi

if [[ -e cmdstan ]]; then
    echo "* Removing the existing installation of CmdStan."
    rm -rf cmdstan
fi

if [[ ${REPO_CLONE} -ne 0 ]]; then
    echo "* Cloning ${REPO_URL} and checking out branch ${REPO_BRANCH}. This may take a few minutes ..."
    git clone --recursive ${REPO_URL} -b ${REPO_BRANCH} ${INSTALL_DIR}
    GIT_RC=$?
    if [[ ${GIT_RC} -ne 0 ]]; then
        error_msg "Error: cloning repository failed."
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

    echo "* Installing CmdStan ${VER} in ${RELDIR}"
    echo "* Downloading ${CS}.tar.gz from GitHub."

    curl -s -OL https://github.com/stan-dev/cmdstan/releases/download/v${VER}/${CS}.tar.gz
    CURL_RC=$?
    if [[ ${CURL_RC} -ne 0 ]]; then
        error_msg "Error: GitHub download failed, curl exited with: ${CURL_RC}"
        exit ${CURL_RC}
    fi
    echo "* Download complete."

    echo "* Unpacking archive ..."

    mkdir cmdstan
    tar xzf ${CS}.tar.gz -C cmdstan/ --strip-components 1
    TAR_RC=$?
    if [[ ${TAR_RC} -ne 0 ]]; then
        error_msg "Error: corrupt download file ${CS}.tar.gz, tar exited with: ${TAR_RC}"
        exit ${TAR_RC}
    fi
    rm ${CS}.tar.gz
fi

pushd cmdstan > /dev/null
echo "* Building CmdStan binaries."
build_cmdstan
if [[ ${REPO_CLONE} -ne 1 ]]; then
  build_example
fi

echo "* Cleaning up ..."
pushd -0 > /dev/null
dirs -c > /dev/null
echo "* Finished installing CmdStan to `ls -Fd ${RELDIR}/cmdstan`"

