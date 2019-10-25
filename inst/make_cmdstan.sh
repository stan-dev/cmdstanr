#!/usr/bin/env bash

# install a cmdstan release into a specified directory
#  - build binaries, compile example model to build model header
#  - symlink downloaded version as "cmdstan"

WIN=0
while getopts ":d:v:j:w" opt; do
  case $opt in
    d) RELDIR="$OPTARG"
    ;;
    v) VER="$OPTARG"
    ;;
    j) JOBS="$OPTARG"
    ;;
    w) WIN=1
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    ;;
  esac
done

if [ -z ${JOBS} ]; then
    JOBS="2"
fi

if [ -z ${RELDIR} ]; then
    RELDIR="$HOME/.cmdstanr"
fi

# if [[ -e ${RELDIR} && ! -d ${RELDIR} ]]; then
#    echo "line 29"
#    echo "cannot install cmdstan, ${RELDIR} is not a directory"
#    exit 1
# fi

if [[ ! -e ${RELDIR} ]]; then
   mkdir -p ${RELDIR}
fi

echo "cmdstan dir: ${RELDIR}"

if [ -z ${VER} ]; then
    TAG=`curl -s https://api.github.com/repos/stan-dev/cmdstan/releases/latest | grep "tag_name"`
    echo $TAG > tmp-tag
    VER=`perl -p -e 's/"tag_name": "v//g; s/",//g' tmp-tag`
    rm tmp-tag
fi
CS=cmdstan-${VER}
echo "cmdstan version: ${VER}"

pushd ${RELDIR} > /dev/null
if [[ -d $cs && -f ${CS}/bin/stanc && -f ${CS}/examples/bernoulli/bernoulli ]]; then
    echo "cmdstan already installed"
    exit 0
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

pushd cmdstan > /dev/null
echo "building cmdstan binaries"
if [[ ${WIN} -ne 0 ]]; then
  mingw32-make -j${JOBS} build examples/bernoulli/bernoulli.exe
else
  make -j${JOBS} build examples/bernoulli/bernoulli
fi
echo "installed ${CS} to ${RELDIR}/cmdstan"

# cleanup
pushd -0 > /dev/null
dirs -c > /dev/null
echo ""
echo "CmdStan installation location: `ls -Fd ${RELDIR}/cmdstan`"
