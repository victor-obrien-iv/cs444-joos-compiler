#!/bin/bash

if [[ "$#" -ne 1 ]]; then echo "usage: <testdir|testfile>"; exit; fi

#testdir=/u/cs444/pub/assignment_testcases/a1/
testsrc=$1

# create a new directory to put the results 
dirnum=0
while [ -d "result$dirnum" ]; do
	dirnum=$((dirnum + 1))
done
rundir=result$dirnum
mkdir $rundir
echo "sending output files to" $rundir

# files to append results
failfile=$rundir/fails.out
passfile=$rundir/passes.out
summaryfile=$rundir/summary.out
touch $failfile $passfile $summaryfile

function runtest {
	testpath=$1		
	tempfile=$(mktemp)
	
	# is this a single file or a directory of them?
	if [ -d $testpath ]; then 
		files=`find "$testpath" -type f -name '*.java'`
		./joosc $files > $tempfile
	else
		./joosc "$testpath" > $tempfile
	fi

	# compare the return code to the file name
	# JX_... => return 0, Je_... => return 42
	retcode=${PIPESTATUS[0]}
	testname=${testpath##*/}	
        if ([ "Je" == ${testname:0:2} ] && [ "$retcode" -ne "42" ]) || ([ "Je" != ${testname:0:2} ] && [ "$retcode" -ne "0" ]); then
		# the return code was incorrect, save the out and append the path to the fail file
		echo $testpath >> $failfile
		cp $tempfile $rundir/$testname.out
        else
		# the test passed, save the path for reference
		echo $testpath >> $passfile
	fi
	rm ${tempfile}
}

if [ -d $testsrc ]; then
	# a directory of tested was passed to the script
	numfiles=$((`\ls -afq $testsrc | wc -l` - 2))
	tests=$testsrc/*
elif [ -f $testsrc ]; then
	# a file of test paths was passed to the script
	numfiles=`wc -l < $testsrc`
	tests=`cat $testsrc`
else
	echo "test source is not a file or directory...?"
	exit
fi

# start a background process for each test
starttime=$SECONDS
for joostest in $tests; do	
	runtest $joostest &
done

# print info and results
echo -ne "awaiting results..."'\t'
wait 
duration=$(( SECONDS - starttime ))
echo "done"
echo -e "
Passes:\t" `wc -l < $passfile` "
Fails:\t" `wc -l < $failfile` "
Total:\t" $numfiles "
Time:\t" $((duration / 60)) "min, " $((duration % 60)) "sec
Date:\t"  `date` >> $summaryfile
echo -ne "sorting results..."'\t'
sort $failfile -o $failfile
sort $passfile -o $passfile
echo "done"
cat $summaryfile
