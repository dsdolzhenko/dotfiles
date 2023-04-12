#!/usr/bin/env bash

ln -sfh "${PWD}/.emacs.d" "${HOME}/.emacs.d"

NONINTERACTIVE=1 brew tap --quiet homebrew/cask-fonts && \
    brew install --quiet --cask font-fira-code
