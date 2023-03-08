# path to look for files in
if [[ $1 != "" && !$1 ]]
then
 path=$1
else
 path=$PWD
fi
# change file types to match yours
nFile=$(find $path \( -name "*.[Rr]" -o -name "*.[Rr]md" -o -name "*.qmd" \) | wc -l)
if [ $nFile == 0 ]
then
  echo -e "\033[0;31m  No files in this folder. Try adding a path as first argument."
else
  total=0
  echo -e "\033[0;36m> Should I show the line count for each file?(y/N)\e[0m"
  read details
  echo -e "\033[0;36m> Should I count comments?(Y/n)\e[0m"
  read count_comment
  for file in `find $path \( -name "*.[Rr]" -o -name "*.[Rr]md" -o -name "*.qmd" \)`; do
    # $numLines is the lines of code in this file
    # sed: only count lines of code that aren't blank lines
    # and (optional) lines that are not commented with #
    if [[ $count_comment == "n" || $count_comment == "N" ]]
    then
      # change '#' for the comment character of your prog language
      numLines=`sed '/^\s*#/d;/^\s*$/d' $file | wc -l`
    else
      numLines=`sed '/^\s*$/d' $file | wc -l`
    fi

    total=$(($total + $numLines))
    # if [ $details == true ]
    if [[ $details == "y" || $details == "Y" ]]
    # if true prints n lines in each files
    then
      echo "  " $numLines "lines in $(basename $file)"
    fi
  done
  # print total number of lines in folder
  echo "  " $total "lines total in '$(basename $path)'"
fi
