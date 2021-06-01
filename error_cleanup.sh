
echo "This removes node_modules, sorry."
rm -rf ./www/node_modules
echo "This removes .next, sorry."
rm -rf ./www/.next
echo "This removes all the stupid .DS_Store MacOS files, not sorry."
find . -type f \( -name ".DS_Store" -o -name "._.DS_Store" \) -delete -print 2>&1 | grep -v "Permission denied"
