#!/usr/bin/env bash

echo "Linking .emacs.d directory..."
ln -sfh "${PWD}/.emacs.d" "${HOME}/.emacs.d"

echo "Installing Fira Code fonts..."
NONINTERACTIVE=1 brew tap --quiet homebrew/cask-fonts && \
    brew install --quiet --cask font-fira-code

if ! alias emacs >/dev/null 2>&1 ; then
    echo "Setup aliases for emacs.."
    cat <<EOF >> "${HOME}/.zshenv"
# Emacs
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias ec="emacs -n"
EOF
fi
