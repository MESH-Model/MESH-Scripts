C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.
 	
!***********************************************************************
!       copyright (c) by Nick Kouwen and Dave Watson 2007
!***********************************************************************

!     rev. Sep. 2008 - Vincent Fortin/Isabelle Dore
!     EOF by ios in function CountDataLinesAfterHeader
!
!     May 2010
!      - 'columnType' was replaced with 'columntype' (D. Deacu)
!
      MODULE EF_Module
	USE EF_ParseUtilities
        
	TYPE Attribute
	CHARACTER*64 name
	CHARACTER*16 units
	INTEGER index, count
	REAL, DIMENSION(:), ALLOCATABLE :: val
	END TYPE Attribute
		
	TYPE EnSimParam
	TYPE(Attribute) :: attList(200)
	INTEGER attCount
	INTEGER  fileFormat
	CHARACTER*8 fileType
	CHARACTER*32 application, writtenBy, creationDate, dataType
	CHARACTER*16 version
	CHARACTER*64 name
	END TYPE EnSimParam
		
	TYPE CoordSysParam
	CHARACTER*12 projection, ellipsoid
	INTEGER zone 
	CHARACTER*8 units
	END TYPE CoordSysParam
	
	TYPE R2CParam
	TYPE(EnSimParam) ep
	TYPE(CoordSysParam) csp
	TYPE(Attribute) attp
	INTEGER  xCount, yCount
	REAL xDelta, yDelta, xOrigin, yOrigin
	REAL angle, unitConv
	INTEGER frameCount
	CHARACTER*32 surveyDate, surveyTime
	END TYPE R2CParam

	TYPE ShedParam
	TYPE(R2CParam) r2cp
	REAL nominalGridSize_AL, contourInterval, elevConversion
	INTEGER classCount, numRiverClasses, totalNumOfGrids
	INTEGER numGridsInBasin, debugGridNo
	CHARACTER*128 sourceFileName
	END TYPE

	TYPE RainParam
	TYPE(R2CParam) r2cp
	INTEGER startJulianDay, endJulianDay, startHour, endHour
	END TYPE
	
	TYPE TempParam
	TYPE(R2CParam) r2cp
	INTEGER startJulianDay, endJulianDay, startHour, endHour
	END TYPE
	
	TYPE GSMParam
	TYPE(R2CParam) r2cp
	END TYPE
	
	TYPE SWEParam
	TYPE(R2CParam) r2cp
	REAL initHeatDeficit
	END TYPE

	TYPE TimeStamp
	INTEGER year,month,day,hour,min,sec,msec
	END TYPE

	Type FrameRecord
	TYPE(TimeStamp) tStamp
	INTEGER frame, step
	INTEGER hour, hoursToNext
	CHARACTER*5 source
	END TYPE

	TYPE TB0Param
	TYPE(EnSimParam) ep
	TYPE(CoordSysParam) csp
	REAL unitConv
	INTEGER deltaT
	END TYPE TB0Param
	
	TYPE FlowParam
	TYPE(TB0Param) tb0p
	INTEGER routingDeltaT
	CHARACTER*2 fillFlag
	END TYPE FlowParam
	
	TYPE ResvParam
	TYPE(TB0Param) tb0p
	END TYPE ResvParam
	
	TYPE ResvinParam
	TYPE(TB0Param) tb0p
	END TYPE ResvinParam
	
	TYPE DivParam
	TYPE(TB0Param) tb0p
	END TYPE DivParam
	
	TYPE TB0ColumnMetaData
	INTEGER colCount
	CHARACTER(64), DIMENSION(:), ALLOCATABLE :: colName
	CHARACTER(16), DIMENSION(:), ALLOCATABLE :: colUnits
	CHARACTER(8), DIMENSION(:), ALLOCATABLE :: colType
	REAL, DIMENSION(:), ALLOCATABLE :: colLocX
	REAL, DIMENSION(:), ALLOCATABLE :: colLocY
	END TYPE TB0ColumnMetaData

	TYPE FlowColumnMetaData
	TYPE(TB0ColumnMetaData) tb0cmd
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff1
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff2
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff3
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff4
	REAL, DIMENSION(:), ALLOCATABLE :: colValue1
	END TYPE FlowColumnMetaData
	
	TYPE ResvColumnMetaData
	TYPE(TB0ColumnMetaData) tb0cmd
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff1
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff2
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff3
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff4
	REAL, DIMENSION(:), ALLOCATABLE :: colCoeff5
        REAL, DIMENSION(:), ALLOCATABLE :: colCoeff6
        REAL, DIMENSION(:), ALLOCATABLE :: colCoeff7
	END TYPE ResvColumnMetaData
	
	TYPE ResvinColumnMetaData
	TYPE(TB0ColumnMetaData) tb0cmd
	REAL, DIMENSION(:), ALLOCATABLE :: colValue1
	END TYPE ResvinColumnMetaData
	
	TYPE DivColumnMetaData
	TYPE(TB0ColumnMetaData) tb0cmd
	REAL   , DIMENSION(:), ALLOCATABLE :: colLocX1
	REAL   , DIMENSION(:), ALLOCATABLE :: colLocY1
	REAL   , DIMENSION(:), ALLOCATABLE :: colValue1
	INTEGER, DIMENSION(:), ALLOCATABLE :: colValue2
	INTEGER, DIMENSION(:), ALLOCATABLE :: colValue3
	INTEGER, DIMENSION(:), ALLOCATABLE :: colValue4
	END TYPE DivColumnMetaData




C		TYPE (TB0Column) columns(10)
C		TYPE(FlowColumn), DIMENSION(:), ALLOCATABLE :: flowCols

	CONTAINS
	SUBROUTINE InitEnSimParam(ep)
	TYPE(EnSimParam), INTENT(INOUT) :: ep
	ep%fileType = "Unknown"
	ep%dataType = "Unknown"
	ep%application = "Unknown"
	ep%version = "Unknown"
	ep%writtenBy = "Unknown"
	ep%creationDate = "Unknown"
	ep%name = "Unknown"	
	ep%attCount = 0		
	END SUBROUTINE InitEnSimParam
	
	SUBROUTINE InitCoordSysParam(csp)
	TYPE(CoordSysParam), INTENT(INOUT) :: csp
	csp%projection = "Cartesian"
	csp%ellipsoid = "Unknown"
	csp%zone = 0
	END SUBROUTINE InitCoordSysParam
	
	SUBROUTINE InitAttribute(attp)
	TYPE(Attribute), INTENT(INOUT) :: attp
	attp%name = "Unknown"
	attp%units = "Unknown"
	attp%index = 0
	attp%count = 0
	END SUBROUTINE InitAttribute
	
	SUBROUTINE InitR2CParam(r2cp)
	TYPE(R2CParam), INTENT(INOUT) :: r2cp
	CALL InitEnSimParam(r2cp%ep)
	CALL InitCoordSysParam(r2cp%csp)
	r2cp%xCount = 0
	r2cp%yCount = 0
	r2cp%xDelta = 0.0
	r2cp%yDelta= 0.0
	r2cp%angle = 0
	r2cp%xOrigin = 0.0
	r2cp%yOrigin = 0.0
	r2cp%frameCount = 0
	r2cp%unitConv = 1.0
	r2cp%surveyDate = "Unknown"
	r2cp%surveyTime = "Unknown"
	END SUBROUTINE InitR2CParam
	
	SUBROUTINE InitShedParam(shdp)
	TYPE(ShedParam), INTENT(INOUT) :: shdp
	CALL InitR2CParam(shdp%r2cp)
	shdp%nominalGridSize_AL = 0.0
	shdp%contourInterval = 0.0
	shdp%classCount = 0
	shdp%numRiverClasses = 0
	shdp%elevConversion = 1.0
	shdp%totalNumOfGrids = 0
	shdp%numGridsInBasin = 0
	shdp%debugGridNo = 0
	END SUBROUTINE InitShedParam
	
	SUBROUTINE InitRainParam(rainp)
	TYPE(RainParam), INTENT(INOUT) :: rainp
	CALL InitR2CParam(rainp%r2cp)
	rainp%startJulianDay = 0
	rainp%startHour = 0
	END SUBROUTINE InitRainParam
	
	SUBROUTINE InitTempParam(tempp)
	TYPE(TempParam), INTENT(INOUT) :: tempp
	CALL InitR2CParam(tempp%r2cp)
	END SUBROUTINE InitTempParam
	
	SUBROUTINE InitGSMParam(gsmp)
	TYPE(GSMParam), INTENT(INOUT) :: gsmp
	CALL InitR2CParam(gsmp%r2cp)
	END SUBROUTINE InitGSMParam
	
	SUBROUTINE InitSWEParam(swep)
	TYPE(SWEParam), INTENT(INOUT) :: swep
	CALL InitR2CParam(swep%r2cp)
	swep%initHeatDeficit = 0.0
	END SUBROUTINE InitSWEParam
	
	SUBROUTINE InitTimeStamp(tStamp)
	TYPE(TimeStamp), INTENT(INOUT) :: tStamp
	tStamp%year = 0
	tStamp%month = 0
	tStamp%day = 0
	tStamp%hour = 0
	tStamp%min = 0
	tStamp%sec = 0
	tStamp%msec = 0
	END SUBROUTINE InitTimeStamp
	
	SUBROUTINE InitFrameRecord(frameRec)
	TYPE(FrameRecord), INTENT(INOUT) :: frameRec
	CALL InitTimeStamp(frameRec%tStamp)
	frameRec%hour = 0
	frameRec%hoursToNext = 0
	frameRec%source = "     "
	END SUBROUTINE InitFrameRecord
	
	SUBROUTINE InitTB0Param(tb0p)
	TYPE(TB0Param), INTENT(INOUT) :: tb0p
	CALL InitEnSimParam(tb0p%ep)
	CALL InitCoordSysParam(tb0p%csp)
	tb0p%deltaT = 0
	tb0p%unitConv = 0
	END SUBROUTINE InitTB0Param
	
	SUBROUTINE InitFlowParam(flowp)
	TYPE(FlowParam), INTENT(INOUT) :: flowp
	CALL InitTB0Param(flowp%tb0p)
	flowp%routingDeltaT = 0
	flowp%fillFlag = "n"
	END SUBROUTINE InitFlowParam
	
	SUBROUTINE InitResvParam(resvp)
	TYPE(ResvParam), INTENT(INOUT) :: resvp
	CALL InitTB0Param(resvp%tb0p)
	END SUBROUTINE InitResvParam
	
	SUBROUTINE InitResvinParam(resvinp)
	TYPE(ResvinParam), INTENT(INOUT) :: resvinp
	CALL InitTB0Param(resvinp%tb0p)
	END SUBROUTINE InitResvinParam
	
	SUBROUTINE InitDivParam(divp)
	TYPE(DivParam), INTENT(INOUT) :: divp
	CALL InitTB0Param(divp%tb0p)
	END SUBROUTINE InitDivParam
	
	SUBROUTINE InitTB0ColumnMetaData(tb0Cols)
	TYPE(TB0ColumnMetaData), INTENT(INOUT) :: tb0Cols
	tb0Cols%colCount = 0
	END SUBROUTINE InitTB0ColumnMetaData
	
	SUBROUTINE InitFlowColumnMetaData(flowcmd)
	TYPE(FlowColumnMetaData), INTENT(INOUT) :: flowcmd
	CALL InitTB0ColumnMetaData(flowcmd%tb0cmd)
	END SUBROUTINE InitFlowColumnMetaData
	
	SUBROUTINE InitResvColumnMetaData(resvcmd)
	TYPE(ResvColumnMetaData), INTENT(INOUT) :: resvcmd
	CALL InitTB0ColumnMetaData(resvcmd%tb0cmd)
	END SUBROUTINE InitResvColumnMetaData
	
	SUBROUTINE InitResvinColumnMetaData(resvincmd)
	TYPE(ResvColumnMetaData), INTENT(INOUT) :: resvincmd
	CALL InitTB0ColumnMetaData(resvincmd%tb0cmd)
	END SUBROUTINE InitResvinColumnMetaData
	
	SUBROUTINE InitDivColumnMetaData(divcmd)
	TYPE(DivColumnMetaData), INTENT(INOUT) :: divcmd
	CALL InitTB0ColumnMetaData(divcmd%tb0cmd)
	END SUBROUTINE InitDivColumnMetaData
	
	INTEGER FUNCTION TimeSpanHours(timeStamp1, timeStamp2)
	TYPE(TimeStamp), INTENT(INOUT) :: timeStamp1, timeStamp2 
	TimeSpanHours = 0
	return
	END FUNCTION TimeSpanHours


	LOGICAL FUNCTION IsLatLong(csp)
	TYPE(CoordSysParam), INTENT(INOUT) :: csp
	
	character*12 projString
	integer strLength
	logical rstat
	
	IsLatLong = .false.
	
	projString = csp%projection
	rstat = ToLowerCase(projString)
	strLength = LEN_TRIM(projString)
	
	if(strLength.eq.7) then
	   if(projString(1:7) .eq. 'latlong') then
	      IsLatLong = .true.
	   endif
	endif
	
	return
	
	END FUNCTION IsLatLong
	

C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseShedParam(header, keyword,
     $                                  keyLen, subString)
	TYPE(ShedParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	if(keyword(1:keyLen) .eq. ':sourcefilename')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%sourceFileName
	      ParseShedParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':nominalgridsize_al')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%nominalGridSize_AL
	      ParseShedParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':contourinterval')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%contourInterval
	      ParseShedParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':classcount')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%classCount
	      ParseShedParam = 1
	   end if
	   return	
	else if(keyword(1:KeyLen) .eq. ':elevconversion')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%elevConversion
	      ParseShedParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':numriverclasses')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%numRiverClasses
	      ParseShedParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':totalnumofgrids')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%totalNumOfGrids
	      ParseShedParam = 1
	   end if
	   return	
	else if(keyword(1:KeyLen) .eq. ':numgridsinbasin')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%numGridsInBasin
	      ParseShedParam = 1
	   end if
	   return						
	else if(keyword(1:KeyLen) .eq. ':debuggridno')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseShedParam = -1
	   else
	      read(value, *) 	header%debugGridNo
	      ParseShedParam = 1
	   end if
	   return						
	end if


! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseShedParam = ParseR2CParam(header%r2cp, keyword,
     &                                 keyLen, subString)
	return

	END FUNCTION ParseShedParam

C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseRainParam(header, keyword,
     &                                  keyLen, subString)
	TYPE(RainParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseRainParam = ParseR2CParam(header%r2cp, keyword,
     &                                 keyLen, subString)

	return

	END FUNCTION ParseRainParam

C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseTempParam(header, keyword,
     &                                  keyLen, subString)
	TYPE(TempParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseTempParam = ParseR2CParam(header%r2cp, keyword,
     &                                 keyLen, subString)

	return

	END FUNCTION ParseTempParam





C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseGSMParam(header, keyword,
     &                                 keyLen, subString)
	TYPE(GSMParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseGSMParam = ParseR2CParam(header%r2cp, keyword,
     &                                keyLen, subString)

	return

	END FUNCTION ParseGSMParam


C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseSWEParam(header, keyword,
     &                                 keyLen, subString)
	TYPE(SWEParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	if(keyword(1:KeyLen) .eq. ':initheatdeficit')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseSWEParam = -1
	   else
	      read(value, *) 	header%initHeatDeficit
	      ParseSWEParam = 1
	   end if
	   return
	endif

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseSWEParam = ParseR2CParam(header%r2cp, keyword,
     &                                keyLen, subString)
	return

	END FUNCTION ParseSWEParam




C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C       
	INTEGER FUNCTION ParseR2CParam(header, keyword,
     &                                 keyLen, subString)
	TYPE(R2CParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseR2CParam = 0 

	if(keyword(1:KeyLen) .eq. ':xcount')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%xCount
	      ParseR2CParam = 1
	   end if
	   return
		
	else if(keyword(1:KeyLen) .eq. ':ycount')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%yCount
	      ParseR2CParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':xdelta')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%xdelta
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':ydelta')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%ydelta
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':xorigin')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%xorigin
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':yorigin')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%yorigin
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':angle')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%angle
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':unitconversion')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%unitConv
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':surveydate')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%surveyDate
	      ParseR2CParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':surveytime')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseR2CParam = -1
	   else
	      read(value, *) 	header%surveyTime
	      ParseR2CParam = 1
	   end if
	   return
	   
	   
	end if
	
! if we're here, then unknown keyword...nothing is assigned yet
! at this point ParseR2CParam = 0
! check if it is a core EnSim parameter
	ParseR2CParam = ParseEnSimParam(header, keyword,
     &                                  keyLen, subString)

	if(ParseR2CParam .ne. 0) return 
			

! if we're here, then unknown keyword...nothing is assigned yet
! check if it is a CoordSys parameter
	ParseR2CParam = ParseCoordSysParam(header, keyword,
     &                                     keyLen, subString)

	return

	END FUNCTION ParseR2CParam


C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseCoordSysParam(header, keyword,
     &                                      keyLen, subString)
	TYPE(R2CParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseCoordSysParam = 0 


	if(keyword(1:KeyLen) .eq. ':projection')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParam = -1
	   else
	      read(value, *) 	header%csp%projection
	      ParseCoordSysParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':ellipsoid')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParam = -1
	   else
	      read(value, *) 	header%csp%ellipsoid
	      ParseCoordSysParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':zone')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParam = -1
	   else
	      read(value, *) 	header%csp%zone
	      ParseCoordSysParam = 1
	   end if
	   return
	end if
	
	return
	
			
	END FUNCTION ParseCoordSysParam

C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseCoordSysParamTB0(header, keyword,
     &                                         keyLen, subString)
	TYPE(TB0Param), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	ParseCoordSysParamTB0 = 0 
	

	if(keyword(1:KeyLen) .eq. ':projection')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParamTB0 = -1
	   else
	      read(value, *) 	header%csp%projection
	      ParseCoordSysParamTB0 = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':ellipsoid')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParamTB0 = -1
	   else
	      read(value, *) 	header%csp%ellipsoid
	      ParseCoordSysParamTB0 = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':zone')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseCoordSysParamTB0 = -1
	   else
	      read(value, *) 	header%csp%zone
	      ParseCoordSysParamTB0 = 1
	   end if
	   return
	end if

	return

	END FUNCTION ParseCoordSysParamTB0



C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseEnSimParam(header, keyword,
     &                                   keyLen, subString)
	TYPE(R2CParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount
	logical rstat

	ParseEnSimParam = 0 

	if(keyword(1:KeyLen) .eq. ':filetype')then ! filetype line
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	      return
	   else
	      header%ep%fileType = value
	      ParseEnSimParam = 1
	      if(value .NE. "r2c") then	! data type
		 write(6,'((A))') 'ERROR in file: '
		 write(6,'(3(A))') 'Expected: r2c', 
     &                             ' Found: ',value
		 ParseEnSimParam = -1
		 return
	      end if
	   endif 
	   
	   subString = tmpString
	   if(SplitLine(subString, value, tmpString) .eq. 0)then ! file format
	      ParseEnSimParam = -1
	      return
	   end if
	   rStat = ToLowerCase(value)
	   if(value .eq. 'ascii')then ! ASCII file format
	      header%ep%fileFormat = 0
	      ParseEnSimParam = 1
	   else if(value .eq. 'binary')then ! BINARY file format
	      header%ep%fileFormat = 1
	      ParseEnSimParam = 1
	   else			! UNKNOWN file format
	      write(6,'(2(A))')' Stopped: ',
     &                         'invalid file format'
	      ParseEnSimParam = -1
	   end if
	   return
		
	else if(keyword(1:KeyLen) .eq. ':application')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%application
	      ParseEnSimParam = 1
	   end if
	   return

	else if(keyword(1:KeyLen) .eq. ':writtenby')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%writtenBy
	      ParseEnSimParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':creationdate')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%creationDate
	      ParseEnSimParam = 1
	   end if
	   return

	else if(keyword(1:KeyLen) .eq. ':datatype')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%dataType
	      ParseEnSimParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':version')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%version
	      ParseEnSimParam = 1
	   end if
	   return


	else if(keyword(1:KeyLen) .eq. ':name')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      read(value, *) 	header%ep%name
	      ParseEnSimParam = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':attributename')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParam = -1
	   else
	      if(IsNumber(value)) then
		 header%ep%attCount = header%ep%attCount+1
		 read(value, *)
     &                header%ep%attList(header%ep%attCount)%index
	      else
		 write(*,'(4(A))')
     &                 'Read ''', TRIM(value), '''',
     &                 ' but expected a number'
		 ParseEnSimParam = -1
		 return
	      end if
	      subString = tmpString
	      if(SplitLine(subString, value, tmpString) .eq. 0)then
		 ParseEnSimParam = -1
	      else
		 read(value, *)
     &                header%ep%attList(header%ep%attCount)%name
		 ParseEnSimParam = 1
	      end if
	   endif
	   return
	end if

	return
			
	END FUNCTION ParseEnSimParam

C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseEnSimParamTB0(header, keyword,
     &                                      keyLen, subString)
	TYPE(TB0Param), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount
	logical rstat
	
	ParseEnSimParamTB0 = 0 

	if(keyword(1:KeyLen) .eq. ':filetype')then ! filetype line
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	      return
	   else
	      header%ep%fileType = value
	      ParseEnSimParamTB0 = 1
	      if(value .NE. "tb0") then	! data type
		 write(6,'((A))') 'ERROR in file: '
		 write(6,'(3(A))') 'Expected: r2c', 
     &                             ' Found: ',value
		 ParseEnSimParamTB0 = -1
		 return
	      end if
	   endif 
				
	   subString = tmpString
	   if(SplitLine(subString, value, tmpString) .eq. 0)then ! file format
	      ParseEnSimParamTB0 = -1
	      return
	   end if
	   rStat = ToLowerCase(value)
	   if(value .eq. 'ascii')then ! ASCII file format
	      header%ep%fileFormat = 0
	      ParseEnSimParamTB0 = 1
	   else if(value .eq. 'binary')then ! BINARY file format
	      header%ep%fileFormat = 1
	      ParseEnSimParamTB0 = 1
	   else			! UNKNOWN file format
	      write(6,'(2(A))')' Stopped: ',
     &                         'invalid file format'
	      ParseEnSimParamTB0 = -1
	   end if
	   return
		
	else if(keyword(1:KeyLen) .eq. ':application')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%application
	      ParseEnSimParamTB0 = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':writtenby')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%writtenBy
	      ParseEnSimParamTB0 = 1
	   end if
	   return

	else if(keyword(1:KeyLen) .eq. ':creationdate')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%creationDate
	      ParseEnSimParamTB0 = 1
	   end if
	   return

	else if(keyword(1:KeyLen) .eq. ':datatype')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%dataType
	      ParseEnSimParamTB0 = 1
	   end if
	   return

	else if(keyword(1:KeyLen) .eq. ':version')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%version
	      ParseEnSimParamTB0 = 1
	   end if
	   return


	else if(keyword(1:KeyLen) .eq. ':name')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      read(value, *) 	header%ep%name
	      ParseEnSimParamTB0 = 1
	   end if
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':attributename')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseEnSimParamTB0 = -1
	   else
	      if(IsNumber(value)) then
		 header%ep%attCount = header%ep%attCount+1
		 read(value, *)
     &                header%ep%attList(header%ep%attCount)%index
	      else
		 write(*,'(4(A))')
     &                 'Read ''', TRIM(value), '''',
     &                 ' but expected a number'
		 ParseEnSimParamTB0 = -1
		 return
	      end if
	      
	      subString = tmpString
	      if(SplitLine(subString, value, tmpString) .eq. 0)then
		 ParseEnSimParamTB0 = -1
	      else
		 read(value, *)
     &                header%ep%attList(header%ep%attCount)%name
		 ParseEnSimParamTB0 = 1
	      end if
	   endif
	   return
	end if
	
	return
			
	END FUNCTION ParseEnSimParamTB0


C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not found in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseFrameLine(frameRec, keyword,
     &                                  keyLen, subString)
	TYPE(FrameRecord), INTENT(INOUT) :: frameRec 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount
	
	ParseFrameLine = 0
C Frame	
	if(keyword(1:KeyLen) .eq. ':frame')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseFrameLine = -1
	      return
	   else
	      read(value, *) frameRec%frame
C				print*,	' '
C				print*,	'frame:',frameRec%frame
	   endif

C Step
	   subString = tmpString
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseFrameLine = -1
	      return
	   else
	      read(value, *) frameRec%step
C				print*,	'step:',frameRec%step
	   end if

C TimeStamp
	   subString = tmpString
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseFrameLine = -1
	      return
	   else
	      if(ParseTimeStamp(frameRec%tStamp, value).eq.0)then 
		 ParseFrameLine = -1
		 return
	      end if
	   end if

CC Hour=
C			subString = tmpString
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseFrameLine = -1
C				return
C			end if
C
CC hour
C			subString = tmpString
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseFrameLine = -1
C				return
C			else
C 				read(value, *) frameRec%hour
C 				print*,	'hour:',frameRec%hour
C			end if
C
CC hour to next
C			subString = tmpString
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseFrameLine = -1
C				return
C			else
C 				read(value, *) frameRec%hoursToNext
C 				print*,	'hoursToNext:',frameRec%hoursToNext
C			end if
C
CC source
C			subString = tmpString
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseFrameLine = -1
C				return
C			else
C 				read(value, *) frameRec%source
C 				print*,	'source:',frameRec%source
C			end if

	   ParseFrameLine = 1
	end if

	return
		
	END FUNCTION ParseFrameLine


C*******************************************************************
C
C
C		Return Value
C		0 = Problem
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseTimeStamp(tStamp, string)
	TYPE(TimeStamp), INTENT(INOUT) :: tStamp 
	CHARACTER*(*), INTENT(INOUT) :: string
	character*128  value
	character*10000 tmpString, localLine
!	integer lineLen, rStat
	integer lineLen
! Dan Princz Changed rStat To logical from integer
	logical rStat
	integer slash, space, colon, point 
	

	localLine = string	! work on local copy of the string
	rStat = Detab(localLine) ! change any tabs to spaces
	localLine = ADJUSTL(localLine) ! get rid of leading spaces
	lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces
	
	
	ParseTimeStamp = 0

C a Date field will have a '/'
	slash = INDEX(localLine(1:lineLen),'/')	! Look for a '/' character

	if(slash.GT.0) then	! we have a date field
C	Extract the year
	   value = localLine(1:slash-1)
	   read(value, *) tStamp%year			
	   localLine = localLine(slash+1:lineLen)
	   localLine = ADJUSTL(localLine) ! get rid of leading spaces
	   lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces
			
C	Extract the month
	   slash = INDEX(localLine(1:lineLen),'/') ! Look for a '/' character
	   if(slash.EQ.0) then
	      return
	   end if
	   value = localLine(1:slash-1)
	   read(value, *) tStamp%month			
	   localLine = localLine(slash+1:lineLen)
	   localLine = ADJUSTL(localLine) ! get rid of leading spaces
	   lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces

C	Extract the day
	   space = INDEX(localLine(1:lineLen),' ') ! Look for a ' ' character
	   if(space.EQ.0) then
	      return
	   end if
	   value = localLine(1:space-1)
	   read(value, *) tStamp%day			
	   localLine = localLine(space+1:lineLen)
	   localLine = ADJUSTL(localLine) ! get rid of leading spaces
	   lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces
	end if
	
	
C Now the time field
		
C	Extract the hour
	colon = INDEX(localLine(1:lineLen),':')	! Look for a ':' character
	if(colon.EQ.0) then
	   return
	end if 
	value = localLine(1:colon-1)
	read(value, *) tStamp%hour			
	localLine = localLine(colon+1:lineLen)
	localLine = ADJUSTL(localLine) ! get rid of leading spaces
	lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces

C	Extract the minute
	if(lineLen.GT.0)then 
	   colon = INDEX(localLine(1:lineLen),':') ! Look for a ':' character
	   if(colon.EQ.0) then
	      value = localLine(1:lineLen)
	      localLine=''
	   else 
	      value = localLine(1:colon-1)
	      localLine = localLine(colon+1:lineLen)
	   endif
	   localLine = localLine(colon+1:lineLen)
	   localLine = ADJUSTL(localLine) ! get rid of leading spaces
	   lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces
	   read(value, *) tStamp%min
	end if				
	
C	Extract the sec.msec
C       first check for msec
	if(lineLen.GT.0)then 
	   point = INDEX(localLine(1:lineLen),'.') ! Look for a '.' character
	   if(point.GT.0) then 
	      value = localLine(point:lineLen)
	      read(value, *) tStamp%msec			
	      localLine = localLine(1:point-1)
	      localLine = ADJUSTL(localLine) ! get rid of leading spaces
	      lineLen = LEN_TRIM(localLine) ! find the new length, excluding trailing spaces
	   endif
C       Now extract the second
	   value = localLine(1:lineLen)
	   read(value, *) tStamp%sec
	end if				
	
	ParseTimeStamp = 1
	
	return
	
	END FUNCTION ParseTimeStamp

C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseTB0Param(header, keyword,
     &                                 keyLen, subString)
	TYPE(TB0Param), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount
	
	ParseTB0Param = 0 

	if(keyword(1:KeyLen) .eq. ':deltat')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseTB0Param = -1
	   else
	      read(value, *) 	header%deltaT
	      ParseTB0Param = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':unitconversion')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseTB0Param = -1
	   else
	      read(value, *) 	header%unitConv
	      ParseTB0Param = 1
	   end if
	   return
	end if
	
! if we're here, then unknown keyword...nothing is assigned yet
! at this point ParseTB0Param = 0
! check if it is a core EnSim parameter
	ParseTB0Param = ParseEnSimParamTB0(header, keyword,
     &                                     keyLen, subString)

	if(ParseTB0Param .ne. 0) return 
			

! if we're here, then unknown keyword...nothing is assigned yet
! check if it is a CoordSys parameter
	ParseTB0Param = ParseCoordSysParamTB0(header, keyword,
     &                                        keyLen, subString)

	return

	END FUNCTION ParseTB0Param


C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseFlowParam(header, keyword,
     &                                  keyLen, subString)
	TYPE(FlowParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

	if(keyword(1:KeyLen) .eq. ':routingdeltat')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseFlowParam = -1
	   else
	      read(value, *) 	header%routingDeltaT
	      ParseFlowParam = 1
	   end if
	   return
	else if(keyword(1:KeyLen) .eq. ':fillflag')then
	   if(SplitLine(subString, value, tmpString) .eq. 0)then
	      ParseFlowParam = -1
	   else
	      read(value, *) 	header%fillFlag
	      ParseFlowParam = 1
	   end if
	   return
	end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks
	
	ParseFlowParam = ParseTB0Param(header%tb0p, keyword,
     &                                 keyLen, subString)
	return

	END FUNCTION ParseFlowParam


C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseResvParam(header, keyword,
     &                                  keyLen, subString)
	TYPE(ResvParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

C		if(keyword(1:KeyLen) .eq. ':routingdeltat')then
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseResvParam = -1
C			else
C				read(value, *) 	header%routingDeltaT
C				ParseResvParam = 1
C			end if
C			return
C		end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseResvParam = ParseTB0Param(header%tb0p, keyword,
     &                                 keyLen, subString)
	return

	END FUNCTION ParseResvParam


C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseResvinParam(header, keyword,
     &                                    keyLen, subString)
	TYPE(ResvinParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

C		if(keyword(1:KeyLen) .eq. ':deltat')then
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseResvinParam = -1
C			else
C				read(value, *) 	header%deltaT
C				ParseResvinParam = 1
C			end if
C		return
C		end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseResvinParam = ParseTB0Param(header%tb0p, keyword,
     &                                   keyLen, subString)
	return

	END FUNCTION ParseResvinParam



C*******************************************************************
C
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
C
	INTEGER FUNCTION ParseDivParam(header, keyword,
     &                                 keyLen, subString)
	TYPE(DivParam), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount

C		if(keyword(1:KeyLen) .eq. ':routingdeltat')then
C			if(SplitLine(subString, value, tmpString) .eq. 0)then
C				ParseDivParam = -1
C			else
C				read(value, *) 	header%routingDeltaT
C				ParseDivParam = 1
C			end if
C			return
C		end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseDivParam = ParseTB0Param(header%tb0p, keyword,
     &                                 keyLen, subString)
	return

	END FUNCTION ParseDivParam


C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseTB0ColumnMetaData(header, keyword,
     &                                          keyLen, subString)
	TYPE(TB0ColumnMetaData), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
	integer lineLen, wordCount,n, ideallocate
	
	tmpString = subString

	if(keyword(1:KeyLen) .eq. ':columnlocationx')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseTB0ColumnMetaData = -1
	      return
	   end if

	   header%colCount = wordCount
	   if(allocated(header%colLocX)) then
	      deallocate(header%colLocX,stat=ideallocate)
	   end if
	   allocate(header%colLocX(wordCount))
	   
	   read(tmpString,*)(header%colLocX(n),n=1,wordCount)


	   ParseTB0ColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':columnlocationy')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseTB0ColumnMetaData = -1
	      return
	   end if
	   
	   header%colCount = wordCount
	   if(allocated(header%colLocY)) then
	      deallocate(header%colLocY,stat=ideallocate)
	   end if
	   allocate(header%colLocY(wordCount))
	   
	   read(tmpString,*)(header%colLocY(n),n=1,wordCount)
	   

	   ParseTB0ColumnMetaData = 1
			
	   return

	else if(keyword(1:KeyLen) .eq. ':columnname')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseTB0ColumnMetaData = -1
	      return
	   end if
	   
	   header%colCount = wordCount
	   if(allocated(header%colName)) then
	      deallocate(header%colName,stat=ideallocate)
	   end if
	   allocate(header%colName(wordCount))
	   
	   read(tmpString,*)(header%colName(n),n=1,wordCount)
	   

	   ParseTB0ColumnMetaData = 1
	   return

c	else if(keyword(1:KeyLen) .eq. ':columnType')then
	else if(keyword(1:KeyLen) .eq. ':columntype')then ! bug fix (D. Deacu)
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseTB0ColumnMetaData = -1
	      return
	   end if
		   
	   header%colCount = wordCount
	   if(allocated(header%colType)) then
	      deallocate(header%colType,stat=ideallocate)
	   end if
	   allocate(header%colType(wordCount))
	   
	   read(tmpString,*)(header%colType(n),n=1,wordCount)
	   

	   ParseTB0ColumnMetaData = 1
	   return
	   

	else if(keyword(1:KeyLen) .eq. ':columnunits')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseTB0ColumnMetaData = -1
	      return
	   end if
	   
	   header%colCount = wordCount
	   if(allocated(header%colUnits)) then
	      deallocate(header%colUnits,stat=ideallocate)
	   end if
	   allocate(header%colUnits(wordCount))
	   
	   read(tmpString,*)(header%colUnits(n),n=1,wordCount)

	   ParseTB0ColumnMetaData = 1
	   return

	end if

	return
	
	END FUNCTION ParseTB0ColumnMetaData
	
C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseFlowColumnMetaData(header, keyword,
     &                                           keyLen, subString)
	TYPE(FlowColumnMetaData), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000  tmpString
C	character*10000  tmpString
	integer lineLen, wordCount,n, ideallocate

	tmpString = subString
              
	if(keyword(1:KeyLen) .eq. ':coeff1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseFlowColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colCoeff1)) then
	      deallocate(header%colCoeff1,stat=ideallocate)
	   end if
	   allocate(header%colCoeff1(wordCount))
	   
	   read(tmpString,*)(header%colCoeff1(n),n=1,wordCount)

	   ParseFlowColumnMetaData = 1
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':coeff2')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseFlowColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colCoeff2)) then
	      deallocate(header%colCoeff2,stat=ideallocate)
	   end if
	   allocate(header%colCoeff2(wordCount))
			
	   read(tmpString,*)(header%colCoeff2(n),n=1,wordCount)

	   ParseFlowColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':coeff3')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseFlowColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colCoeff3)) then
	      deallocate(header%colCoeff3,stat=ideallocate)
	   end if
	   allocate(header%colCoeff3(wordCount))
	   read(tmpString,*)(header%colCoeff3(n),n=1,wordCount)

	   ParseFlowColumnMetaData = 1
	   return
	else if(keyword(1:KeyLen) .eq. ':coeff4')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseFlowColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colCoeff4)) then
	      deallocate(header%colCoeff4,stat=ideallocate)
	   end if
	   allocate(header%colCoeff4(wordCount))
	   
	   read(tmpString,*)(header%colCoeff4(n),n=1,wordCount)
			

	   ParseFlowColumnMetaData = 1
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':value1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseFlowColumnMetaData = -1
	      return
	   end if

	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colValue1)) then
	      deallocate(header%colValue1,stat=ideallocate)
	   end if
	   allocate(header%colValue1(wordCount))

	   read(tmpString,*)(header%colValue1(n),n=1,wordCount)
	   

	   ParseFlowColumnMetaData = 1
	   return


	end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks
	
	ParseFlowColumnMetaData = ParseTB0ColumnMetaData(header%tb0cmd, 
     &                            keyword, keyLen, subString)
	return


	return

	END FUNCTION ParseFlowColumnMetaData

C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseResvColumnMetaData(header, keyword,
     &                                           keyLen, subString)
	TYPE(ResvColumnMetaData), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
C	character*10000 tmpString
	integer lineLen, wordCount,n, ideallocate

	tmpString = subString
	
	if(keyword(1:KeyLen) .eq. ':coeff1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(.NOT.allocated(header%colCoeff1)) then
	      allocate(header%colCoeff1(wordCount))
	   end if
	   read(tmpString,*)(header%colCoeff1(n),n=1,wordCount)

	   ParseResvColumnMetaData = 1
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':coeff2')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(.NOT.allocated(header%colCoeff2)) then
	      allocate(header%colCoeff2(wordCount))
	   end if
	   
	   read(tmpString,*)(header%colCoeff2(n),n=1,wordCount)

	   ParseResvColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':coeff3')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvColumnMetaData = -1
	      return
	   end if

	   header%tb0cmd%colCount = wordCount
	   deallocate(header%colCoeff3,stat=ideallocate)
	   allocate(header%colCoeff3(wordCount))
	   
	   read(tmpString,*)(header%colCoeff3(n),n=1,wordCount)

	   ParseResvColumnMetaData = 1
	   return
	else if(keyword(1:KeyLen) .eq. ':coeff4')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   deallocate(header%colCoeff4,stat=ideallocate)
	   allocate(header%colCoeff4(wordCount))
	   
	   read(tmpString,*)(header%colCoeff4(n),n=1,wordCount)

	   ParseResvColumnMetaData = 1
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':coeff5')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   deallocate(header%colCoeff5,stat=ideallocate)
	   allocate(header%colCoeff5(wordCount))

	   read(tmpString,*)(header%colCoeff5(n),n=1,wordCount)

	   ParseResvColumnMetaData = 1
	   return
	   
        else if(keyword(1:KeyLen) .eq. ':coeff6')then
           wordCount = CountWords(tmpString)
           if(wordCount.le. 0)then
              ParseResvColumnMetaData = -1
              return
           end if

           header%tb0cmd%colCount = wordCount
           deallocate(header%colCoeff6,stat=ideallocate)
           allocate(header%colCoeff6(wordCount))

           read(tmpString,*)(header%colCoeff6(n),n=1,wordCount)

           ParseResvColumnMetaData = 1
           return

        else if(keyword(1:KeyLen) .eq. ':coeff7')then
           wordCount = CountWords(tmpString)
           if(wordCount.le. 0)then
              ParseResvColumnMetaData = -1
              return
           end if

           header%tb0cmd%colCount = wordCount
           deallocate(header%colCoeff7,stat=ideallocate)
           allocate(header%colCoeff7(wordCount))

           read(tmpString,*)(header%colCoeff7(n),n=1,wordCount)

           ParseResvColumnMetaData = 1
           return
	   
	end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseResvColumnMetaData = ParseTB0ColumnMetaData(header%tb0cmd, 
     &                            keyword, keyLen, subString)
	return

	END FUNCTION ParseResvColumnMetaData


C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseResvinColumnMetaData(header, keyword,
     &                                             keyLen, subString)
	TYPE(ResvinColumnMetaData), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
C	character*10000 tmpString
	integer lineLen, wordCount,n, ideallocate

	tmpString = subString

	if(keyword(1:KeyLen) .eq. ':value1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseResvinColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   deallocate(header%colValue1,stat=ideallocate)
	   allocate(header%colValue1(wordCount))

	   read(tmpString,*)(header%colValue1(n),n=1,wordCount)
	   

	   ParseResvinColumnMetaData = 1
	   return

	end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseResvinColumnMetaData = ParseTB0ColumnMetaData( 
     &            header%tb0cmd,keyword, keyLen, subString)
	return

	END FUNCTION ParseResvinColumnMetaData


C*******************************************************************
C
C		Return Value
C		-1 = Problem
C		0 = keyword not foung in Type
C               1 = Successfully assigned
C
	INTEGER FUNCTION ParseDivColumnMetaData(header, keyword,
     &                                          keyLen, subString)
	TYPE(DivColumnMetaData), INTENT(INOUT) :: header 
	INTEGER, INTENT(IN) :: keyLen
	CHARACTER*(*), INTENT(INOUT) :: keyword, subString
	character*128  value
	character*10000 tmpString
C	character*10000 tmpString
	integer lineLen, wordCount,n, ideallocate

	tmpString = subString
	
	if(keyword(1:KeyLen) .eq. ':columnlocationx1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(.NOT.allocated(header%colLocX1)) then
	      allocate(header%colLocX1(wordCount))
	   end if
	   read(tmpString,*)(header%colLocX1(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':columnlocationy1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(.NOT.allocated(header%colLocY1)) then
	      allocate(header%colLocY1(wordCount))
	   end if
	   read(tmpString,*)(header%colLocY1(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':value1')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colValue1)) then
	      deallocate(header%colValue1,stat=ideallocate)
	   end if
	   allocate(header%colValue1(wordCount))
!	   if(.NOT.allocated(header%colValue1)) then
!	      allocate(header%colValue1(wordCount))
!	   end if
	   read(tmpString,*)(header%colValue1(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return
	   
	else if(keyword(1:KeyLen) .eq. ':value2')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colValue2)) then
	      deallocate(header%colValue2,stat=ideallocate)
	   end if
	   allocate(header%colValue2(wordCount))
!	   if(.NOT.allocated(header%colValue2)) then
!	      allocate(header%colValue2(wordCount))
!	   end if
	   read(tmpString,*)(header%colValue2(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':value3')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colValue3)) then
	      deallocate(header%colValue3,stat=ideallocate)
	   end if
	   allocate(header%colValue3(wordCount))
!	   if(.NOT.allocated(header%colValue3)) then
!	      allocate(header%colValue3(wordCount))
!	   end if
	   read(tmpString,*)(header%colValue3(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return

	else if(keyword(1:KeyLen) .eq. ':value4')then
	   wordCount = CountWords(tmpString)
	   if(wordCount.le. 0)then
	      ParseDivColumnMetaData = -1
	      return
	   end if
	   
	   header%tb0cmd%colCount = wordCount
	   if(allocated(header%colValue4)) then
	      deallocate(header%colValue4,stat=ideallocate)
	   end if
	   allocate(header%colValue4(wordCount))
!	   if(.NOT.allocated(header%colValue4)) then
!	      allocate(header%colValue4(wordCount))
!	   end if
	   read(tmpString,*)(header%colValue4(n),n=1,wordCount)

	   ParseDivColumnMetaData = 1
	   return

	   
	end if

! if we're here, then keyword not assigned yet
! let's look to the parent blocks

	ParseDivColumnMetaData  = ParseTB0ColumnMetaData(header%tb0cmd, 
     &                            keyword, keyLen, subString)
	return

	END FUNCTION ParseDivColumnMetaData


C*******************************************************************
C
C
C
C
	SUBROUTINE LoadAttributeData(epHeader, xCount, yCount, unitNum)
	TYPE(EnSimParam), INTENT(INOUT) :: epHeader
	INTEGER xCount, yCount, unitNum
	INTEGER index, valCount, n, xi, yi, ios
 
	valCount = xCount*yCount
		
	do n=1, epHeader%attCount
	   ALLOCATE( epHeader%attList(n)%val(valCount))
			
	   do yi=1,yCount
	      index = (yi-1)*xCount
	      read(unitNum,*,iostat=ios)
     &            (epHeader%attList(n)%val(index+xi),xi=1,xCount)
c	print*,(epHeader%attList(n)%val(index+xi),xi=1,xCount)
c      pause
	   end do
	end do
			
	END SUBROUTINE LoadAttributeData


C*******************************************************************
C
C
	LOGICAL FUNCTION IsFileTypeR2C(fName)
		
	character*(*) :: fName 
	character*256 tmpString
	integer strLength
	logical rstat
	
	tmpString = fName
	
	IsFileTypeR2C = .false.

	rstat = ToLowerCase(tmpString)
	strLength = LEN_TRIM(tmpString)
	
	if(tmpString(strLength-2:strLength) .eq. 'r2c') then
	   IsFileTypeR2C = .true.
	endif

	return

	END FUNCTION IsFileTypeR2C

C*******************************************************************
C
C
	LOGICAL FUNCTION IsFileTypeTB0(fName)
		
	character*(*) :: fName 
	character*256 tmpString
	integer strLength
	logical rstat

	tmpString = fName
	
	IsFileTypeTB0 = .false.

	rstat = ToLowerCase(tmpString)
	strLength = LEN_TRIM(tmpString)
	
	if(tmpString(strLength-2:strLength) .eq. 'tb0') then
	   IsFileTypeTB0 = .true.
	endif

	return

	END FUNCTION IsFileTypeTB0

C*******************************************************************
C
C
	LOGICAL FUNCTION GetBoolean(string)
	
	character*(*) :: string 
	character*256 tmpString
	integer strLength
	logical rstat

	tmpString = string
	
	GetBoolean = .false.

	rstat = ToLowerCase(tmpString)
	strLength = LEN_TRIM(tmpString)
	
	if((tmpString(1:1) .eq. 'y') .or.
     &     (tmpString(1:1) .eq. 't'))  then
	   GetBoolean = .true.
	endif

	return

	END FUNCTION GetBoolean



C*******************************************************************
C
C
C
C
	SUBROUTINE GoToStartOfData(unitNum)
	INTEGER unitNum, ios, lineLen, wordCount, keyLen
	logical rstat, foundEndHeader
	character*10000 tmpString
	character*10000 line, subString
	character*128 keyword

	foundEndHeader = .false.
	line = '#'
		
	do WHILE((.NOT.foundEndHeader) .AND.
     &          ((line(1:1) .eq. '#') .OR.
     &           (line(1:1) .eq. ':') .OR.
     &           (LEN_TRIM(line) .eq. 0))) 	

	   read(UNIT=unitNum, FMT='((A))', iostat=ios) line ! read a line

	   rStat = Detab(line)	! replace tabs with spaces
	   line = ADJUSTL(line)	! Get rid of leading white space
	   lineLen = LEN_TRIM(line) ! Find the length excluding trailing spaces

	   if(line(1:1) .eq. ':')then
	      wordCount = SplitLine(line, keyword, subString) ! find keyword
	      rStat = ToLowerCase(keyword)
	      KeyLen = LEN_TRIM(keyword)

	      if(keyword(1:KeyLen) .eq. ':endheader')then
		 foundEndHeader = .TRUE.
	      endif
	   end if
	end do
			
	END SUBROUTINE GoToStartOfData




C*******************************************************************
C
C
	INTEGER FUNCTION CountDataLinesAfterHeader(unitNum)
	
	INTEGER unitNum, ios, lineLen
	character*256 line
	logical rstat

	CountDataLinesAfterHeader = 0

	CALL GoToStartOfData(unitNum)
	
C Now count the data lines
	read(unit=unitNum, FMT='((A))', iostat=ios) line ! read a line

C            DO WHILE(.NOT.EOF(unitNum))        !vfo
	DO WHILE(ios.EQ.0)	!vfo
	   rStat = Detab(line)	! replace tabs with spaces
	   line = ADJUSTL(line)	! Get rid of leading white space		
	   lineLen = LEN_TRIM(line) ! Find the length excluding trailing spaces

	   if(lineLen.gt.0 .and. line(1:1) .ne. '#')then
	      CountDataLinesAfterHeader=CountDataLinesAfterHeader+1
	   end if

	   read(unit=unitNum, FMT='((A))', iostat=ios) line	
	END DO
	
	END FUNCTION CountDataLinesAfterHeader

	END MODULE EF_Module
	
