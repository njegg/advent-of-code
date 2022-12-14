#/bin/bash

set -e

echo -n "What's todays day? > " 
read day

if [ ${#day} -eq 1 ]
    then
        dir="d0"$day
    else 
        dir="d"$day
fi

mkdir $dir
cd $dir

touch $day"_"{1.hs,2.hs,i.txt,s.txt}

