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
if [ ! -e .next ]; then
echo "---"
echo "---"
echo "#Running npm run build, hold on to your butts."
echo "---"
echo "---"
yarn build
else 
echo "---"
echo "---"
echo "#.next folder already exists, let me save you from having to wait it out."
echo "---"
echo "---"
fi
yarn watch