#!/bin/bash
if [ "$#" -eq 3 ]; then
	files=`ls -R $1`
else
	files=`ls $1`
fi

for f in $files
do
	cat $f | grep "$2" >& /dev/null
	if [ "$?" -eq 0 ]; then
		echo $f
	fi
done
