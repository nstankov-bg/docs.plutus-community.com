#!/bin/sh

if [ ! -e node_modules ]; then
#Running the installer, hold on to your butts.
echo "---"
echo "---"
echo "#Running the installer, hold on to your butts."
echo "---"
echo "---"
yarn install
else 
echo "---"
echo "---"
echo "#node_modules folder already exists, let me save you from having to wait it out."
fi
echo "#running a local build"
yarn dev