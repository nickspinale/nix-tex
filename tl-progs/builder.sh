#!/bin/sh

source $stdenv/setup

tar xf $src

mkdir wat
cd wat
TL_INSTALL_DEST=$out TL_CONFIGURE_ARGS="--without-x" ../texlive-20150521-source/Build
