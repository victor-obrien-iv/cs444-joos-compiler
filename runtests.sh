#!/bin/bash

if [[ "$#" -ne 2 ]]; then echo "usage: <testdir> <outfile>"; exit; fi

#testdir=/u/cs444/pub/assignment_testcases/a1/
testdir=$1
numfails=0
failfiles=
outfile=$2
> $outfile # clears the test.out file
progress=1
numfiles=$((`\ls -afq $testdir | wc -l` - 2))

echo "fails/progress/total"
for testfile in $testdir*; do	
	echo -ne $numfails "/" $progress "/" $numfiles'\t'$testfile'\t'
	./joosc $testfile > /dev/null
	if ([ "Je" == ${testfile:${#testdir}:2} ] && [ "$?" -ne "42" ]) || ([ "Je" != ${testfile:${#testdir}:2} ] && [ "$?" -ne "0" ]); then
		numfails=$((numfails + 1))
		echo $testfile >> $outfile
		echo "failed" 	
	else 
		echo 
	fi
	progress=$((progress + 1)) 
done
echo 
echo "summary: " $numfails'/'$numfiles " failures"
