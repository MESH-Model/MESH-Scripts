#!/bin/bash

#If the following directory does not exists create new directory

###### SET THE CONVERSION PARAMETERS HERE ######

	source_dir="/home/melshamy/MESH/66_WRF_Processing/wrf2r2c_v6/"
    export 	CONST_DIR=$source_dir		# WRF Constant File
	ln -sf /project/6008034/Model_Output/WRF/WRF4KM_CA/wrfout_d01_constant.nc $source_dir/
	export 	IN_DIR="/project/6008034/Model_Output/WRF/WRF4KM_CA/HYDRO/CTRL/"	# WRF Input Data directory
	
	START_YEAR=2003
	END_YEAR=2003
	
for (( YEAR1=$START_YEAR; YEAR1<=$END_YEAR; YEAR1++ ))
   	do
        export 	xDelta=0.125
        export 	yDelta=0.125
        export 	xCount=308
        export 	yCount=156
        export 	xOrigin=-140.50
        export 	yOrigin=51.0
        export 	YEAR1
        export 	MONTHS_Start=1
        export  MONTHS_End=1

		Output="/project/6008034/Model_Output/AEP_MESH/TEST/WRF_CTRL_Bow/${YEAR1}"

		rm -rf $Output/precipitation

		if [ ! -d $Output/precipitation ]; then
			mkdir -p $Output/precipitation
			cp -r ${source_dir}header_* $Output/precipitation/
			cp ${source_dir}join.sh $Output/precipitation/
			cp ${source_dir}wrf2rc_precip.wgt.ncl $Output/precipitation/wrf2rc_precipitation.ncl
			sed -i "28i :AttributeName           PRECIPITATIONE" $Output/precipitation/header_info.sh
			sed -i "29i :AttributeUnit           mm/hour" $Output/precipitation/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/precipitation/wrf2rc_precipitation.ncl
			touch $Output/precipitation/precipitation.log
		fi

############## START SUBMITTING CONVERSION SCRIPTS ###########	
		module load ncl

		  cd $Output/precipitation/
		  bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		  ncl $Output/precipitation/wrf2rc_precipitation.ncl >& precipitation.log &
		  #PID=$!
		  echo -e "${yellow} Processing start for PRECIPITATION"  ${YEAR1}
				
		echo " Still Processing for" ${YEAR1} 
		# wait $PID	
		cd ../
	
	done
