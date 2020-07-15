#!/bin/sh
git clone https://github.com/adobe-fonts/source-code-pro.git /tmp/source-code-pro
mkdir ~/.local/share/fonts
mv /tmp/source-code-pro/TTF/*.ttf ~/.local/share/fonts
rm -rf /tmp/source-code-pro
fc-cache -f -v
fc-list
