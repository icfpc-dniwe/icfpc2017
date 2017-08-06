#!/bin/bash

ARGC=($#)
if [ "$ARGC" -eq 0 ]; then
  echo "Usage: $0 server|dniwe|submission submission.tar.gz"
  exit 1
fi

HOME="/root"
SSH_PATH="$HOME/.ssh"

PREBUILT_SRV="lamduct"
PREBUILT_SRV_URL="icfpcontest2017.github.io/static/lamduct-0.3"

INSTALL="apt-get install -y"
PACKAGES=(git vim nano cabal-install zlibc zlib1g-dev m4 opam wget)

MODE=$1
[ "$MODE" == "dniwe" ] && DEPLOY_DNIWE=true || DEPLOY_DNIWE=false
[ "$MODE" == "server" ] && DEPLOY_SRV=true || DEPLOY_SRV=false
[ "$MODE" == "submission" ] && DEPLOY_SUBMISSION=true || DEPLOY_SUBMISSION=false

apt-get update

for pkg in ${PACKAGES[@]}; do
  $INSTALL $pkg
done

if [ "$DEPLOY_DNIWE" = true ]; then
  echo "Deploying DNIWE repo"

  echo "abbradar.net,128.68.201.44 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFdB9PwNb3fZE6U25WVkapLGf96qkpCJtURBBpqyrRKG" >> "$SSH_PATH/known_hosts"
  echo "abbradar.moe,178.140.126.136 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFdB9PwNb3fZE6U25WVkapLGf96qkpCJtURBBpqyrRKG" >> "$SSH_PATH/known_hosts"
  git clone git@abbradar.moe:icfpc2017
  
  cd icfpc2017/solution
  cabal sandbox init
  cabal update
  cabal install --dependencies-only
  cabal build
  cd -
fi

if [ "$DEPLOY_SRV" = true ]; then
  echo "Deploying offline server"
  echo "mode not supported"
  exit 1
  #cd /
  #opam init -y
  #opam pin add lambda-duct https://github.com/icfpcontest2017/lambda-duct.git -y
fi

if [ "$DEPLOY_SUBMISSION" = true ]; then
  SUB_NAME=$2
  SUB_BASENAME="${SUB_NAME##*/}"

  echo "Deploying submission $SUB_BASENAME"

  wget -c "$SUB_NAME"
  tar -xvf "$SUB_BASENAME"
  cd "${SUB_BASENAME%%.*}"

  if [ ! -f "../$PREBUILT_SRV" ]; then
    echo "Prebuilt srv not found: downloading from $PREBUILT_SRV_URL"
    wget -c "$PREBUILT_SRV_URL" -O "$PREBUILT_SRV"
    chmod +x "$PREBUILT_SRV"
  else
    cp "../$PREBUILT_SRV" "."
  fi

  cd -
fi
