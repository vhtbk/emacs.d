#!/bin/bash

set -e

# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  OS="macos"
elif [ -f /etc/debian_version ]; then
  OS="debian"
elif [ -f /etc/fedora-release ]; then
  OS="fedora"
else
  echo "Unsupported OS. Manual installation required."
  exit 1
fi

echo "Detected OS: $OS"

# Install dependencies
case $OS in
  debian)
    echo "Installing Debian/Ubuntu dependencies."
    sudo apt update
    sudo apt install -y clangd curl build-essential emacs fd-find git libgccjit-12-dev pandoc python3-pip ripgrep snapd texlive-base
    echo ""
    read -p "Would you like to install JetBrains font? (y/N): " install_font
    if [[ "$install_font" =~ ^[Yy]$ ]]; then
      sudo apt install -y fonts-jetbrains-mono
    else
      echo "Skipping font setup."
    fi
    echo ""
    read -p "Would you like to install pyright with Snap? (y/N): " install_pyright
    if [[ "$install_pyright" =~ ^[Yy]$ ]]; then
      sudo snap install pyright --classic
    else
      echo "Skipping pyright setup."
    fi
    echo ""
    read -p "Would you like to install Marksman with Snap? (y/N): " install_marksman
    if [[ "$install_marksman" =~ ^[Yy]$ ]]; then
      sudo snap install marksman
    else
      echo "Skipping Marksman setup."
    fi
    ;;
  fedora)
    echo "Installing Fedora dependencies."
    sudo dnf install -y cmake emacs fd-find git libgccjit-devel pandoc python3-pip ripgrep texlive-scheme-basic
    sudo dnf groupinstall -y "Development Tools"
    echo ""
    read -p "Would you like to install JetBrains font? (y/N): " install_font
    if [[ "$install_font" =~ ^[Yy]$ ]]; then
      sudo dnf copr enable -y elxreno/jetbrains-mono-fonts
      sudo dnf install -y jetbrains-mono-fonts
    else
      echo "Skipping font setup."
    fi
    echo ""
    read -p "Would you like to install pyright? (y/N): " install_pyright
    if [[ "$install_pyright" =~ ^[Yy]$ ]]; then
      sudo dnf install -y nodejs npm 
      sudo npm install -g pyright
    else
      echo "Skipping pyright setup."
    fi
    ;;
  macos)
    if ! command -v brew &> /dev/null; then
      echo "Installing Homebrew."
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    echo "Installing macOS dependencies."
    brew tap d12frosted/emacs-plus
    brew install emacs-plus@29 --with-native-comp
    brew install coreutils fd git libgccjit pandoc ripgrep
    echo ""
    read -p "Would you like to install JetBrains and Hack fonts? (y/N): " install_font
    if [[ "$install_font" =~ ^[Yy]$ ]]; then
      brew install --cask font-jetbrains-mono font-hack
    else
      echo "Skipping font setup."
    fi
    echo ""
    read -p "Would you like to install pyright? (y/N): " install_pyright
    if [[ "$install_pyright" =~ ^[Yy]$ ]]; then
      brew install pyright
    else
      echo "Skipping pyright setup."
    fi
    echo ""
    read -p "Would you like to install Marksman? (y/N): " install_marksman
    if [[ "$install_marksman" =~ ^[Yy]$ ]]; then
      brew install marksman
    else
      echo "Skipping Marksman setup."
    fi
    ;;
esac

echo ""
read -p "Would you like to install Ollama for local AI features? (y/N): " install_ollama

if [[ "$install_ollama" =~ ^[Yy]$ ]]; then
  if ! command -v ollama &> /dev/null; then
    echo "Installing Ollama."
    if [[ "$OS" == "macos" ]]; then
      brew install ollama
      brew services start ollama
    else
      curl -fsSL https://ollama.com/install.sh | sh
    fi
  else
    echo "Ollama is already installed."
  fi

  echo "Pulling Llama3 model."
  ollama pull llama3:8b
else
  echo "Skipping AI setup."
fi

echo -e "\n"
echo "Installation complete! Open Emacs to finish the setup."
echo ""
echo "Run these two commands:"
echo ""
echo " 1.  M-x vhtbk/install-grammars"
echo " 2.  M-x nerd-icons-install-fonts"
echo ""
