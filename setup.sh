#### INSTALLATIONS

echo "## Installing Homebrew & Cask, and helpful software"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew analytics off

brew tap caskroom/cask
brew doctor

brew cask install iterm2
brew cask install vlc
brew cask install slate
cat ./brew_packages.txt | xargs brew install
