#!/bin/bash

# check for -j flag
#parallelflag=''
while getopts 'j' flag; do
	case "${flag}" in
		j) parallelflag='true'; shift ;;
		*) error "Unexpected option ${flag}" ;;
	esac
done

# check that there is only one arg
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

	# if the parallel flag isn't defined, echo progress
	[ -z "$parallelflag" ] && echo -ne $numfails "/" $progress "/" $numfiles'\t'$testpath'\t'

	# is this a single file or a directory of them?
	if [ -d $testpath ]; then
		files=`find "$testpath" -type f -name '*.java'`
		./joosc $files > $tempfile
	else
		./joosc "$testpath" > $tempfile
	fi
	retcode=${PIPESTATUS[0]}

	# compare the return code to the file name
	# JX_... => return 0, Je_... => return 42
	testname=${testpath##*/}
        if ([ "Je" == ${testname:0:2} ] && [ "$retcode" -eq "42" ]) || ([ "Je" != ${testname:0:2} ] && [ "$retcode" -eq "0" ]); then
		[ -z "$parallelflag" ] && echo ""
		# the test passed, save the path for reference
		echo $testpath >> $passfile
        else
		[ -z "$parallelflag" ] && echo "failed" && numfails=$((numfails + 1))
		# the return code was incorrect, save the output and append the path to the fail file
		echo $testpath >> $failfile
		cp $tempfile $rundir/$testname.out
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

if [ -n "$parallelflag" ]; then
	echo $parallelflag
	# start a background process for each test
	starttime=$SECONDS
	for joostest in $tests; do
		runtest $joostest &
	done

	echo -ne "awaiting results..."'\t'
	wait
else
	echo "fails / progress / total"
	# run each test sequentially
	progress=1
	numfails=0
	starttime=$SECONDS
	for joostest in $tests; do
		runtest $joostest
		progress=$((progress + 1))
	done
fi

# print info and results
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
