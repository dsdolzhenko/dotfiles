#!/usr/bin/env bash

EMACS_BRANCH=emacs-29

NONINTERACTIVE=1 brew install --quiet \
     automake autoconf      `# required by autotools build scripts (./autogen.sh and ./configure)` \
     pkg-config             `# otherwise ./configure can't find required dependencies` \
     gnutls libxml2 texinfo `# basic emacs dependencies` \
     gcc libgccjit          `# required by --with-native-compilation` \
     imagemagick            `# required by --with-imagemagick` \
     jansson                `# required by --with-json` \
     tree-sitter            `# required by --with-tree-sitter`

mkdir -p deps && cd deps

[ ! -d emacs ]       && git clone https://git.savannah.gnu.org/git/emacs.git
[ ! -d emacs-icons ] && git clone https://github.com/SavchenkoValeriy/emacs-icons.git

cp emacs-icons/Emacs.icns \
   emacs/nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns

cd emacs                                  && \
    git switch --quiet "${EMACS_BRANCH}"  && \
    git pull --quiet                      && \
    ./autogen.sh                          && \
    ./configure                              \
        --with-native-compilation            \
        --with-tree-sitter                   \
        --with-imagemagick                   \
        --with-json                       && \
    make -j$(nproc)                       && \
    make install

open deps/emacs/nextstep
