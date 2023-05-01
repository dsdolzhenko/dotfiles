#!/usr/bin/env bash

NONINTERACTIVE=1 brew install wget jq fzf fd git

/usr/local/opt/fzf/install --all --no-bash --no-fish

echo -e "\n# fzf" >> ~/.zshenv
echo -e "export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'" >> ~/.zshenv
