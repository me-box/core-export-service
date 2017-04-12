#!/bin/bash

for cnt in 30 100 300 1000 #3000 10000
do
    for payload in 0 1000
    do
	echo "----- will issue" $cnt "requests each with" $payload "bytes data to export -----"
	./lp_perf.sh $cnt $payload
	dir=c${cnt}_p${payload}
	mkdir $dir
	mv queue_count request_delay throughput $dir
	echo "----- done -----"
	echo
	sleep 2
    done
done
