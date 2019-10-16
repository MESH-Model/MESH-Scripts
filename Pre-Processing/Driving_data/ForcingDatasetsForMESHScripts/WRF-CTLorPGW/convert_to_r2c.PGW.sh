#!/bin/bash

#If the following directory does not exists create new directory

###### SET THE CONVERSION PARAMETERS HERE ######

	source_dir="/home/melshamy/MESH/66_WRF_Processing/wrf2r2c_v6/"
    export 	CONST_DIR=$source_dir		# WRF Constant File
	ln -sf /project/6008034/Model_Output/WRF/WRF4KM_CA/wrfout_d01_constant.nc $source_dir/
	export 	IN_DIR="/project/6008034/Model_Output/WRF/WRF4KM_CA/HYDRO/PGW/"	# WRF Input Data directory
	
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
        export 	MONTHS_Start=8
        export  MONTHS_End=12

		Output="/project/6008034/Model_Output/WRF/WRF4KM_CA/WRF_PGW_MRB/${YEAR1}"

		rm -rf $Output/wind
		rm -rf $Output/glw
		rm -rf $Output/humidity
		rm -rf $Output/pressure
		rm -rf $Output/precipitation
		rm -rf $Output/sw
		rm -rf $Output/T2

		if [ ! -d $Output/wind ]; then
			mkdir -p $Output/wind  
			cp -r ${source_dir}header_* $Output/wind/
			cp ${source_dir}join.sh $Output/wind/
			cp ${source_dir}wrf2rc_wind.ncl $Output/wind/ 
			sed -i "28i :AttributeName           wind_speed_10m" $Output/wind/header_info.sh
			sed -i "29i :AttributeUnit           m/s" $Output/wind/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/wind/wrf2rc_wind.ncl
			touch $Output/wind/wind.log
		fi

		if [ ! -d $Output/glw ]; then
			mkdir -p $Output/glw
			cp -r ${source_dir}header_* $Output/glw/
			cp ${source_dir}join.sh $Output/glw/
			cp ${source_dir}wrf2rc_glw.ncl $Output/glw/
			sed -i "28i :AttributeName           DOWNWARD LONG WAVE FLUX AT GROUND SURFACE" $Output/glw/header_info.sh
			sed -i "29i :AttributeUnit           W m-2" $Output/glw/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/glw/wrf2rc_glw.ncl
			touch $Output/glw/glw.log
		fi

		if [ ! -d $Output/humidity ]; then
			mkdir -p $Output/humidity
			cp -r ${source_dir}header_* $Output/humidity/
			cp ${source_dir}join.sh $Output/humidity/
			cp ${source_dir}wrf2rc_humidity.ncl $Output/humidity/
			sed -i "28i :AttributeName           Humidity at 2M" $Output/glw/header_info.sh
			sed -i "29i :AttributeUnit           kg kg-2" $Output/humidity/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/humidity/wrf2rc_humidity.ncl
			touch $Output/humidity/humidity.log
		fi

		if [ ! -d $Output/pressure ]; then
			mkdir -p $Output/pressure
			cp -r ${source_dir}header_* $Output/pressure/
			cp ${source_dir}join.sh $Output/pressure/
			cp ${source_dir}wrf2rc_pressure.ncl $Output/pressure/
			sed -i "28i :AttributeName           SURFACE PRESSURE" $Output/pressure/header_info.sh
			sed -i "29i :AttributeUnit           Pa" $Output/pressure/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/pressure/wrf2rc_pressure.ncl
			touch $Output/pressure/pressure.log
		fi


		if [ ! -d $Output/precipitation ]; then
			mkdir -p $Output/precipitation
			cp -r ${source_dir}header_* $Output/precipitation/
			cp ${source_dir}join.sh $Output/precipitation/
			cp ${source_dir}wrf2rc_precip.ncl $Output/precipitation/wrf2rc_precipitation.ncl
			sed -i "28i :AttributeName           PRECIPITATIONE" $Output/precipitation/header_info.sh
			sed -i "29i :AttributeUnit           mm/hour" $Output/precipitation/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/precipitation/wrf2rc_precipitation.ncl
			touch $Output/precipitation/precipitation.log
		fi


		if [ ! -d $Output/sw ]; then
			mkdir -p $Output/sw
			cp -r ${source_dir}header_* $Output/sw/
			cp ${source_dir}join.sh $Output/sw/
			cp ${source_dir}wrf2rc_sw.ncl $Output/sw/
			sed -i "28i :AttributeName           DOWNWARD SHORT WAVE FLUX AT GROUND SURFACE" $Output/sw/header_info.sh
			sed -i "29i :AttributeUnit           W m-2" $Output/sw/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/sw/wrf2rc_sw.ncl
			touch $Output/sw/sw.log
		fi

		if [ ! -d $Output/T2 ]; then
			mkdir -p $Output/T2
			cp -r ${source_dir}header_* $Output/T2/
			cp ${source_dir}join.sh $Output/T2/
			cp ${source_dir}wrf2rc_t2.ncl $Output/T2/
			sed -i "28i :AttributeName           SURFACE TEMP at 2M" $Output/T2/header_info.sh
			sed -i "29i :AttributeUnit           K" $Output/T2/header_info.sh
			sed -r -i 's/(Opt@DstGridType).*/\1'\=\ \"${xDelta}deg'\"/' $Output/T2/wrf2rc_t2.ncl
			touch $Output/T2/t2.log
		fi

############# START SUBMITTING CONVERSION SCRIPTS ###########	
		module load ncl
		 cd $Output/wind/
		 bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta 
		 ncl $Output/wind/wrf2rc_wind.ncl >& wind.log & 
		 PID=$!
		 echo -e "${yellow} Processing start for wind" ${YEAR1}
		
		 cd $Output/glw/
		 bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		 ncl $Output/glw/wrf2rc_glw.ncl >& glw.log &
		 echo -e "${yellow} Processing start for GLW"  ${YEAR1}

		 cd $Output/humidity/
		 bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		 ncl $Output/humidity/wrf2rc_humidity.ncl >& humidity.log &
		 echo -e "${yellow} Processing start for HUMIDITY"  ${YEAR1}

		 cd $Output/pressure/
		 bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		 ncl $Output/pressure/wrf2rc_pressure.ncl >& pressure.log &
		 echo -e "${yellow} Processing start for PRESSURE"  ${YEAR1}

		  cd $Output/precipitation/
		  bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		  ncl $Output/precipitation/wrf2rc_precipitation.ncl >& precipitation.log &
		  #PID=$!
		  echo -e "${yellow} Processing start for PRECIPITATION"  ${YEAR1}
		
		  cd $Output/sw/
		  bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		  ncl $Output/sw/wrf2rc_sw.ncl >& sw.log &
		  echo -e "${yellow} Processing start for Short Wave Radiation"  ${YEAR1}
		
		 cd $Output/T2/
		 bash ./header_info.sh $xOrigin $yOrigin $xCount $yCount $xDelta $yDelta
		 ncl $Output/T2/wrf2rc_t2.ncl >& t2.log &
		 echo -e "${yellow} Processing start for T2"  ${YEAR1}
			
		echo " Still Processing for" ${YEAR1} 
		wait $PID	
		cd ../
	
	done
