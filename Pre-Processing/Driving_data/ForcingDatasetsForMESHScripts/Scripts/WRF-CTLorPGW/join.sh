#!/bin/bash

if [ -f foo.out ] ; then
rm foo.out
fi

START=1
#END=$(($1-1))
END=$1
echo $END

filname=$2

for (( days=$START; days<10; days++ ))


#for days in {$START..$END}
do
for hrs in {1..24}
do
hrs=$(($hrs-1))
cat GPRINT0${days}_${hrs} >> foo.out
echo $hrs
done
echo $days
done

 
for (( days=10; days<=$END; days++ ))


#for days in {$START..$END}
do
for hrs in {1..24}
do
hrs=$(($hrs-1))
cat GPRINT${days}_${hrs} >> foo.out
echo $hrs
done
echo $days
done


#### Add header to the file ....
sed -i -e '2{x;G};1{h;rheader_file.txt' -e 'd}' foo.out

sed -e '/Frame/s/ *//' < foo.out > $filname

rm -rf GPRINT* foo.out
