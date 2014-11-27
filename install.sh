#!/bin/sh
set -x

prefix="$1"
if [ -z "$prefix" ]; then
  prefix="/usr/local"
fi

install fbfrog "${prefix}/bin"
cp -R include/fbfrog "${prefix}/include"
