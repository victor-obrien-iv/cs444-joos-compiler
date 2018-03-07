#!/bin/bash

function printusage {
echo "
        To run joos tests
                runtest.sh <flags>

        -t <testdir|testfile>
                Run the batch of tests specified in testdir or testfile

        -j <numtasks>,
                Run tests in parallel with <numtasks> number of background processes at a time

        -l <lib>,
                Include library <lib> in each compilation

        -s <test>,
                Run a single test
"
exit
}

while getopts 'ht:j:l:s:' flag; do
	case "${flag}" in
		h)
			printusage
		;;
		t)
			# set testsrc to the passes path if it exists, otherwise treat it as a public test folder
			if [[ -d "${OPTARG}" ]] || [[ -f "${OPTARG}" ]]; then
				testsrc=${OPTARG}
			else
				testsrc=/u/cs444/pub/assignment_testcases/${OPTARG}
			fi
		;;
		j)
			parallelnum=${OPTARG}
		;;
		l)
			if [[ -d "${OPTARG}" ]]; then
				libpath=${OPTARG}
			else
				libpath=/u/cs444/pub/stdlib/${OPTARG}
			fi
			[[ ! -d "$libpath" ]] && echo "could not find library $libpath" && exit
			libfiles=`find "$libpath" -type f -name '*.java'`
			echo "using library files from $libpath"
		;;
		s)
			singletest=${OPTARG}
		;;
		*) echo "Unexpected option ${flag}" && exit ;;
	esac
done

# if given a single test to run
if [[ -n "$singletest" ]]; then
	# is this a single file or a directory of them?
        if [ -d $singletest ]; then
                files=`find "$singletest" -type f -name '*.java'`
                echo "./joosc" $files $libfiles
                ./joosc $files $libfiles
        else
                echo "./joosc" $singletest $libfiles
                ./joosc $singletest $libfiles
        fi
	exit
fi

# if no test src was specified, exit
[[ -z "$testsrc" ]] && printusage

# create a new directory to put the results
dirnum=0
while [ -d "result$dirnum" ]; do
	dirnum=$((dirnum + 1))
done
rundir=result$dirnum
mkdir $rundir

# files to append results
failfile=$rundir/fails.out
passfile=$rundir/passes.out
summaryfile=$rundir/summary.out
touch $failfile $passfile $summaryfile

function runtest {
	testpath=$1
	tempfile=$(mktemp)

	# if the parallel flag isn't defined, echo progress
	[ -z "$parallelnum" ] && echo -ne $numfails "/" $progress "/" $numfiles'\t'$testpath'\t'

	# is this a single file or a directory of them?
	if [ -d $testpath ]; then
		files=`find "$testpath" -type f -name '*.java'`
		echo "./joosc" $files $libfiles > $tempfile
		./joosc $files $libfiles &>> $tempfile
	else
		echo "./joosc" $testpath $libfiles > $tempfile
		./joosc $testpath $libfiles &>> $tempfile
	fi
	retcode=${PIPESTATUS[0]}

	# compare the return code to the file name
	# JX_... => return 0, Je_... => return 42
	testname=${testpath##*/}
        if ([ "Je" == ${testname:0:2} ] && [ "$retcode" -eq "42" ]) || ([ "Je" != ${testname:0:2} ] && [ "$retcode" -eq "0" ]); then
		[ -z "$parallelnum" ] && echo ""
		# the test passed, save the path for reference
		echo $testpath >> $passfile
        else
		[ -z "$parallelnum" ] && echo "failed" && numfails=$((numfails + 1))
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
fi

if [ -n "$parallelnum" ]; then
	echo `date`
	# start a background process for each test
	starttime=$SECONDS
	count=0
	progress=0
	for joostest in $tests; do
		runtest $joostest &
		count=$((count + 1))
		if [[ $count -eq $parallelnum ]]; then
			wait
			progress=$((progress + count))
			echo $progress "/" $numfiles
			count=0
		fi
	done

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
echo "output files in" $rundir/
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
