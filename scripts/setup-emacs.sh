#!/usr/bin/env zsh

echo "Linking .emacs.d directory..."
ln -sfh "${PWD}/.emacs.d" "${HOME}/.emacs.d"

echo "Installing JetBrains Mono fonts..."
NONINTERACTIVE=1 brew tap --quiet homebrew/cask-fonts && \
    brew install --quiet --cask font-jetbrains-mono

echo "Installing cmake..."
brew install --quiet cmake # required by vterm package to compile its native part

if ! alias ec >/dev/null 2>&1 ; then
    echo "Setup aliases for emacs..."
    cat <<EOF >> "${HOME}/.zshenv"
# Emacs
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
EOF
fi
