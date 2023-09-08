# Function to correct my misuse of `\()` instead of `function()`
file2fix=($(grep -l -r --include="*.R" "\\\("))
if (( ${#file2fix[*]} > 0 ))
then
echo "There are ${#file2fix[*]} files to fix.\nFix them now?(Y/n)"
read fix
if [ "$fix" != "n" ] && [ "$fix" != "N" ]
then
echo "Keep backup files? (y/N)"
read keep
for x in ${file2fix[@]}
do
echo "fixing file '$x'"
sed -i".tmp" -r -e 's/\\(\([A-z0-9_.]+\))/function\1/g' $x
if [ "keep" != "y" ] && [ "$keep" != "Y" ]
then
rm "${x}.tmp"
fi
echo "file '$x' fixed"
done
fi
fi
