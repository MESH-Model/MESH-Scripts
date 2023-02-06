#!/bin/bash
# create_runsvs_mod.sh
# This script creates runsvs_mod.ftn90 from list_of_bus_variables_used_by_svs.txt

#SVS_FULL=.

NTILES=5
INFILE=$1
if [ "$2" == "" ]
then
	RUNSVSDIR=$SVS_FULL/src
else
	RUNSVSDIR=$2
fi
RUNSVS_MOD=$RUNSVSDIR/runsvs_mod_sa_mesh.ftn90
#
# Determine the number of variables
BUSNVAR=`cat $INFILE | wc -l`

# Ready to create the source code of the module
# ---------------------------------------------

cat >$RUNSVS_MOD <<EOF
module runsvs_mod

implicit none

integer, parameter :: runsvs_busnvar = $BUSNVAR
integer, parameter :: runsvs_ntiles = $NTILES
integer NG
integer, parameter :: INDX_SOIL    =  1
integer, parameter :: INDX_GLACIER =  2
integer, parameter :: INDX_WATER   =  3
integer, parameter :: INDX_ICE     =  4
integer, parameter :: INDX_AGREGE  =  5
integer, parameter :: INDX_URB     =  6
integer, parameter :: INDX_MAX     =  6
integer, dimension(runsvs_busnvar), save :: runsvs_buspos
integer, dimension(runsvs_busnvar), save :: runsvs_buslev
integer, save :: runsvs_busdim
character(len = 20), dimension(runsvs_busnvar), save :: runsvs_busvarname
character(len = 4), dimension(runsvs_busnvar), save :: runsvs_busoutname
EOF
awk -F";" '
BEGIN{
	pos=1}
{
        printf("integer, save :: %s \n",$1,pos);
	pos = pos+$5}
' $INFILE >> $RUNSVS_MOD
cat >toto.txt <<EOF

contains

subroutine svs_bus_init(NGRIDCELLS)

  implicit none

  integer NGRIDCELLS

  NG = NGRIDCELLS
EOF

cat toto.txt >> $RUNSVS_MOD

awk -F";" '
BEGIN{
	pos=1;
	printf("  runsvs_buspos = (/")}
NR>1 && NR%7==0{
        printf(",& \n ")}
NR>1 && NR%7>0{
        printf(", ")}
{
	printf("(%d-1)*NG+1",pos);
	pos=pos+$5
}
END{
	printf("/)\n");
	printf("  runsvs_busdim = %d*NG\n",pos)}
' $INFILE >> $RUNSVS_MOD

awk -F";" '
BEGIN{
        printf("  runsvs_buslev = (/")}
NR>1 && NR%10==0{
        printf(",& \n ")}
NR>1 && NR%10>0{
        printf(", ")}
{
        printf("%d",$5)
}
END{
        printf("/)\n")}
' $INFILE >> $RUNSVS_MOD

awk -F";" '
BEGIN{
        printf("  runsvs_busvarname = [ character(len=20) :: &\n")}
NR>1 && NR%6==0{
        printf(",& \n ")}
NR>1 && NR%6>0{
        printf(", ")}
{
        printf("\"%s\"",gensub(/ /,"","g",$1))
}
END{
        printf("]\n")}
' $INFILE >> $RUNSVS_MOD

awk -F";" '
BEGIN{
        printf("  runsvs_busoutname = [ character(len=4) :: & \n")}
NR>1 && NR%8==0{
        printf(",& \n ")}
NR>1 && NR%8>0{
        printf(", ")}
{
        printf("\"%s\"",$3)
}
END{
        printf("]\n")}
' $INFILE >> $RUNSVS_MOD

awk -F";" '
BEGIN{
	pos=1}
{
        printf("  %s = (%d-1)*NG+1\n",$1,pos);
	pos = pos+$5}
' $INFILE >> $RUNSVS_MOD


echo "end subroutine" >> $RUNSVS_MOD

echo "end module runsvs_mod" >> $RUNSVS_MOD

rm -f toto.txt
